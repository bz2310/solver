use crate::card::*;
use crate::mutex_like::*;

#[cfg(feature = "bincode")]
use bincode::{Decode, Encode};

pub(crate) const PLAYER_OOP: u8 = 0;
pub(crate) const PLAYER_IP: u8 = 1;
pub(crate) const PLAYER_CHANCE: u8 = 2; // only used with `PLAYER_CHANCE_FLAG`
pub(crate) const PLAYER_MASK: u8 = 3;
pub(crate) const PLAYER_CHANCE_FLAG: u8 = 4; // chance_player = PLAYER_CHANCE_FLAG | prev_player
pub(crate) const PLAYER_TERMINAL_FLAG: u8 = 8;
pub(crate) const PLAYER_FOLD_FLAG: u8 = 24;

/// Available actions of the postflop game.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "bincode", derive(Decode, Encode))]
pub enum StudAction {
    /// (Default value)
    #[default]
    None,

    /// Fold action.
    Fold,

    /// Check action.
    Check,

    /// Call action.
    Call,

    /// Bet action
    Bet,

    /// Raise action
    Raise,

    /// CR-someday: implement. All-in action with a specified amount.
    /// AllIn(i32),

    /// Chance action with a card ID, i.e., the dealing of a turn or river card.
    Chance(Card),
}

/// An enum representing the board state.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
#[cfg_attr(feature = "bincode", derive(Decode, Encode))]
pub enum StudBoardState {
    #[default]
    ThirdStreet = 0,
    FourthStreet = 1,
    FifthStreet = 2,
    SixthStreet = 3,
    SeventhStreet = 4
}

/// A struct containing the game tree configuration.

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "bincode", derive(Decode, Encode))]
pub struct StudTreeConfig {
    /// Initial state of the game tree (upcards).
    pub initial_state: StudBoardState,

    /// Starting pot size from antes. Must be greater than 0
    pub starting_pot_from_antes: i32,

    /// CR-someday. Implement all-ins in stud. Initial effective stack. Must be greater than `0`.
    /// pub effective_stack: i32,

    /// CR-someday. Implement rake in stud.
    /// Rake rate. Must be between `0.0` and `1.0`, inclusive.
    /// pub rake_rate: f64,

    /// Rake cap. Must be non-negative.
    /// pub rake_cap: f64,

    // Num bets per street. Must be positive.
    pub bets_cap: i32,
    pub small_bet_size: i32,
    pub big_bet_size: i32
}

/// A struct representing an abstract game tree.
///
/// An [`ActionTree`] does not distinguish between possible chance events (i.e., the dealing of turn
/// and river cards) and treats them as the same action.
#[derive(Default)]
pub struct StudActionTree {
    config: StudTreeConfig,
    root: Box<MutexLike<StudActionTreeNode>>,
    history: Vec<StudAction>,
}

#[derive(Default)]
#[cfg_attr(feature = "bincode", derive(Decode, Encode))]
pub(crate) struct StudActionTreeNode {
    pub(crate) player_whose_action: u8,
    pub(crate) board_state: BoardState,
    pub(crate) amount_wagered_so_far_this_game: i32,
    pub(crate) actions_so_far_this_game: Vec<StudAction>,
    pub(crate) children: Vec<MutexLike<ActionTreeNode>>,
}

struct StudBuildTreeInfo {
    prev_action: StudAction,
    num_bets: i32,
///    oop_call_flag: bool,
}

impl StudActionTree {
    /// Creates a new [`ActionTree`] with the specified configuration.
    #[inline]
    pub fn new(config: StudTreeConfig) -> Result<Self, String> {
        Self::check_config(&config)?;
        let mut ret = Self {
            config,
            ..Default::default()
        };
        ret.build_tree();
        Ok(ret)
    }

    /// Obtains the configuration of the game tree.
    #[inline]
    pub fn config(&self) -> &StudTreeConfig {
        &self.config
    }

    /// Returns a list of all terminal nodes that should not be.
    #[inline]
    pub fn invalid_terminals(&self) -> Vec<Vec<Action>> {
        let mut ret = Vec::new();
        let mut line = Vec::new();
        Self::invalid_terminals_recursive(&self.root.lock(), &mut ret, &mut line);
        ret
    }

    /// Moves back to the root node.
    #[inline]
    pub fn back_to_root(&mut self) {
        self.history.clear();
    }

    /// Obtains the current action history.
    #[inline]
    pub fn history(&self) -> &[Action] {
        &self.history
    }

    /// Applies the given action history from the root node.
    #[inline]
    pub fn apply_history(&mut self, history: &[Action]) -> Result<(), String> {
        self.back_to_root();
        for &action in history {
            self.play(action)?;
        }
        Ok(())
    }

    /// Returns whether the current node is a terminal node.
    #[inline]
    pub fn is_terminal_node(&self) -> bool {
        self.current_node_skip_chance().is_terminal()
    }

    /// Returns whether the current node is a chance node.
    #[inline]
    pub fn is_chance_node(&self) -> bool {
        self.current_node().is_chance() && !self.is_terminal_node()
    }

    /// Returns the available actions for the current node.
    ///
    /// If the current node is a chance node, returns possible actions after the chance event.
    #[inline]
    pub fn available_actions(&self) -> &[Action] {
        &self.current_node_skip_chance().actions
    }

    /// Plays the given action. Returns `Ok(())` if the action is valid.
    ///
    /// The `action` must be one of the possible actions at the current node.
    /// If the current node is a chance node, the chance action is automatically played before
    /// playing the given action.
    #[inline]
    pub fn play(&mut self, action: Action) -> Result<(), String> {
        let node = self.current_node_skip_chance();
        if !node.actions.contains(&action) {
            return Err(format!("Action `{action:?}` is not available"));
        }

        self.history.push(action);
        Ok(())
    }

    /// Undoes the last action. Returns `Ok(())` if the action is successfully undone.
    #[inline]
    pub fn undo(&mut self) -> Result<(), String> {
        if self.history.is_empty() {
            return Err("No action to undo".to_string());
        }

        self.history.pop();
        Ok(())
    }

    /// Returns the total bet amount of each player (OOP, IP).
    #[inline]
    pub fn total_bet_amount(&self) -> [i32; 2] {
        let info = BuildTreeInfo::new(self.config.effective_stack);
        self.total_bet_amount_recursive(&self.root.lock(), &self.history, info)
    }

    /// Returns the reference to the current node.
    #[inline]
    fn current_node(&self) -> &StudActionTreeNode {
        unsafe {
            let mut node = &*self.root.lock() as *const StudActionTreeNode;
            for action in &self.history {
                while (*node).is_chance() {
                    node = &*(*node).children[0].lock();
                }
                let index = (*node).actions.iter().position(|x| x == action).unwrap();
                node = &*(*node).children[index].lock();
            }
            &*node
        }
    }

    /// Returns the reference to the current node skipping chance nodes.
    #[inline]
    fn current_node_skip_chance(&self) -> &StudActionTreeNode {
        unsafe {
            let mut node = self.current_node() as *const StudActionTreeNode;
            while (*node).is_chance() {
                node = &*(*node).children[0].lock();
            }
            &*node
        }
    }

    /// Checks the configuration.
    /// CR-soon: add more checks here
    #[inline]
    fn check_config(config: &StudTreeConfig) -> Result<(), String> {
        if config.starting_pot <= 0 {
            return Err(format!(
                "Starting pot must be positive: {}",
                config.starting_pot_from_antes
            ));
        }
        Ok(())
    }

    /// Builds the action tree.
    #[inline]
    fn build_tree(&mut self) {
        let mut root = self.root.lock();
        *root = StudActionTreeNode::default();
        root.board_state = self.config.initial_state;
        self.build_tree_recursive(&mut root, BuildTreeInfo);
        ///CR-someday: if we implement all ins make sure to pass in the new eff stack here
    }

    /// Recursively builds the action tree.
    fn build_tree_recursive(&self, node: &mut StudActionTreeNode, info: BuildTreeInfo) {
        if node.is_terminal() {
            // do nothing
        } else if node.is_chance() {
            let next_state = match node.board_state {
                StudBoardState::ThirdStreet => StudBoardState::FourthStreet,
                StudBoardState::FourthStreet => StudBoardState::FifthStreet,
                StudBoardState::FifthStreet => StudBoardState::SixthStreet,
                StudBoardState::SixthStreet => StudBoardState::SeventhStreet,
                StudBoardState::SeventhStreet => unreachable!(),
            };
            // Cr-someday: this is another spot to think about all-ins
            let next_player = PLAYER_OOP

            node.actions.push(Action::Chance(0));
            node.children.push(MutexLike::new(StudActionTreeNode {
                player: next_player,
                board_state: next_state,
                amount: node.amount,
                ..Default::default()
            }));

            self.build_tree_recursive(
                &mut node.children[0].lock(),
                info.create_next(0, Action::Chance(0)),
            );
        } else {
            self.push_actions(node, &info);
            for (action, child) in node.actions.iter().zip(node.children.iter()) {
                self.build_tree_recursive(
                    &mut child.lock(),
                    info.create_next(node.player, *action),
                );
            }
        }
    }

    /// Pushes all possible actions to the given node.
    fn push_actions(&self, node: &mut StudActionTreeNode, info: &StudBuildTreeInfo) {
        let mut actions = Vec::new();

        match info.prev_action {
            Action::Chance(_) | Action::None => 
                actions.push(Action::Check);
                actions.push(Action::Bet);,
            Action::Check =>
                actions.push(Action::Check);     actions.push(Action::Bet);,
            Action::Bet =>
                actions.push(Action::Fold) ;
                actions.push(Action::Call) ;
                if num_bets_on_this_street < bets_cap { actions.push(Action::Raise); },
            
        }
         
        }


        // remove duplicates
        actions.sort_unstable();
        actions.dedup();

        let player_after_call = match node.board_state {
            StudBoardState::SeventhStreet => PLAYER_TERMINAL_FLAG,
            _ => PLAYER_CHANCE_FLAG | player,
        };

        let player_after_check = match player {
            PLAYER_OOP => opponent,
            _ => player_after_call,
        };

        // push actions
        for action in actions {
            let mut amount = node.amount;
            let next_player = match action {
                Action::Fold => PLAYER_FOLD_FLAG | player,
                Action::Check => player_after_check,
                Action::Call => {
                    amount += to_call;
                    player_after_call
                }
                Action::Bet | Action::Raise => {
                    amount += to_call;
                    opponent
                }
                _ => panic!("Unexpected action: {action:?}"),
            };

            node.actions.push(action);
            node.children.push(MutexLike::new(StudActionTreeNode {
                player: next_player,
                board_state: node.board_state,
                amount,
                ..Default::default()
            }));
        }

        node.actions.shrink_to_fit();
        node.children.shrink_to_fit();
    }

    /// Recursive function to enumerate all invalid terminal nodes.
    fn invalid_terminals_recursive(
        node: &StudActionTreeNode,
        result: &mut Vec<Vec<Action>>,
        line: &mut Vec<Action>,
    ) {
        if node.is_terminal() {
            // do nothing
        } else if node.children.is_empty() {
            result.push(line.clone());
        } else if node.is_chance() {
            Self::invalid_terminals_recursive(&node.children[0].lock(), result, line)
        } else {
            for (&action, child) in node.actions.iter().zip(node.children.iter()) {
                line.push(action);
                Self::invalid_terminals_recursive(&child.lock(), result, line);
                line.pop();
            }
        }
    }

    /// Recursive function to add a given line to the tree.
    fn add_line_recursive(
        &self,
        node: &mut StudActionTreeNode,
        line: &[Action],
        was_removed: bool,
        info: BuildTreeInfo,
    ) -> Result<bool, String> {
        if line.is_empty() {
            return Err("Empty line".to_string());
        }

        if node.is_terminal() {
            return Err("Unexpected terminal node".to_string());
        }

        if node.is_chance() {
            return self.add_line_recursive(
                &mut node.children[0].lock(),
                line,
                was_removed,
                info.create_next(0, Action::Chance(0)),
            );
        }

        let action = line[0];
        let search_result = node.actions.binary_search(&action);

        let player = node.player;
        let opponent = node.player ^ 1;

        if line.len() > 1 {
            if search_result.is_err() {
                return Err(format!("Action does not exist: {action:?}"));
            }

            return self.add_line_recursive(
                &mut node.children[search_result.unwrap()].lock(),
                &line[1..],
                was_removed,
                info.create_next(player, action),
            );
        }

        if search_result.is_ok() {
            return Err(format!("Action already exists: {action:?}"));
        }

        let player_stack = info.stack[player as usize];
        let opponent_stack = info.stack[opponent as usize];
        let prev_amount = info.prev_amount;
        let to_call = player_stack - opponent_stack;

        let max_amount = opponent_stack + prev_amount;
        let min_amount = (prev_amount + to_call).clamp(1, max_amount);

        let mut is_replaced = false;
 
        let player_after_call = match node.board_state {
            StudBoardState::River => PLAYER_TERMINAL_FLAG,
            _ => PLAYER_CHANCE_FLAG | player,
        };

        let player_after_check = match player {
            PLAYER_OOP => opponent,
            _ => player_after_call,
        };

        let mut amount = node.amount;
        let next_player = match action {
            Action::Fold => PLAYER_FOLD_FLAG | player,
            Action::Check => player_after_check,
            Action::Call => {
                amount += to_call;
                player_after_call
            }
            Action::Bet(_) | Action::Raise(_) | Action::AllIn(_) => {
                amount += to_call;
                opponent
            }
            _ => panic!("Unexpected action: {action:?}"),
        };

        let index = search_result.unwrap_err();
        node.actions.insert(index, action);
        node.children.insert(
            index,
            MutexLike::new(StudActionTreeNode {
                player: next_player,
                board_state: node.board_state,
                amount,
                ..Default::default()
            }),
        );

        node.actions.shrink_to_fit();
        node.children.shrink_to_fit();

        self.build_tree_recursive(
            &mut node.children[index].lock(),
            info.create_next(player, action),
        );

        Ok(is_replaced)
    }

    /// Recursive function to remove a given line from the tree.
    fn remove_line_recursive(node: &mut StudActionTreeNode, line: &[Action]) -> Result<(), String> {
        if line.is_empty() {
            return Err("Empty line".to_string());
        }

        if node.is_terminal() {
            return Err("Unexpected terminal node".to_string());
        }

        if node.is_chance() {
            return Self::remove_line_recursive(&mut node.children[0].lock(), line);
        }

        let action = line[0];
        let search_result = node.actions.binary_search(&action);
        if search_result.is_err() {
            return Err(format!("Action does not exist: {action:?}"));
        }

        if line.len() > 1 {
            return Self::remove_line_recursive(
                &mut node.children[search_result.unwrap()].lock(),
                &line[1..],
            );
        }

        let index = search_result.unwrap();
        node.actions.remove(index);
        node.children.remove(index);

        node.actions.shrink_to_fit();
        node.children.shrink_to_fit();

        Ok(())
    }

    /// Recursive function to compute total bet amount for each player.
    fn total_bet_amount_recursive(
        &self,
        node: &StudActionTreeNode,
        line: &[Action],
        info: BuildTreeInfo,
    ) -> [i32; 2] {
        if line.is_empty() || node.is_terminal() {
            let stack = self.config.effective_stack;
            return [stack - info.stack[0], stack - info.stack[1]];
        }

        if node.is_chance() {
            return self.total_bet_amount_recursive(&node.children[0].lock(), line, info);
        }

        let action = line[0];
        let search_result = node.actions.binary_search(&action);
        if search_result.is_err() {
            panic!("Action does not exist: {action:?}");
        }

        let index = search_result.unwrap();
        let next_info = info.create_next(node.player, action);
        self.total_bet_amount_recursive(&node.children[index].lock(), &line[1..], next_info)
    }
}

impl StudActionTreeNode {
    #[inline]
    fn is_terminal(&self) -> bool {
        self.player & PLAYER_TERMINAL_FLAG != 0
    }

    #[inline]
    fn is_chance(&self) -> bool {
        self.player & PLAYER_CHANCE_FLAG != 0
    }
}

impl StudBuildTreeInfo {
    #[inline]
    fn new(stack: i32) -> Self {
        Self {
            prev_action: Action::None,
            num_bets_on_this_street: 0,
        }
    }

    #[inline]
    fn create_next(&self, player: u8, action: Action) -> Self {
        let mut num_bets = self.num_bets;

        match action {
            Action::Check => {
                oop_call_flag = false;
            }
            Action::Call => {
                num_bets = 0;
            }
            Action::Bet | Action::Raise => {
                let to_call = stack[player as usize] - stack[player as usize ^ 1];
                num_bets += 1;
                stack[player as usize] -= amount - prev_amount + to_call;
                prev_amount = amount;
            }
            _ => {}
        }

        BuildTreeInfo {
            prev_action: action,
            num_bets,
            allin_flag,
            oop_call_flag,
            stack,
            prev_amount,
        }
    }
}

/// Returns the number of action nodes of [flop, turn, river].
pub(crate) fn count_num_action_nodes(node: &StudActionTreeNode) -> [u64; 3] {
    let mut ret = [0, 0, 0];
    count_num_action_nodes_recursive(node, 0, &mut ret);
    if ret[1] == 0 {
        ret = [0, 0, ret[0]];
    } else if ret[2] == 0 {
        ret = [0, ret[0], ret[1]];
    }
    ret
}

fn count_num_action_nodes_recursive(node: &StudActionTreeNode, street: usize, count: &mut [u64; 3]) {
    count[street] += 1;
    if node.is_terminal() {
        // do nothing
    } else if node.is_chance() {
        count_num_action_nodes_recursive(&node.children[0].lock(), street + 1, count);
    } else {
        for child in &node.children {
            count_num_action_nodes_recursive(&child.lock(), street, count);
        }
    }
}

fn merge_bet_actions(actions: Vec<Action>, pot: i32, offset: i32, param: f64) -> Vec<Action> {
    const EPS: f64 = 1e-12;

    let get_amount = |action: Action| match action {
        Action::Bet(amount) | Action::Raise(amount) | Action::AllIn(amount) => amount,
        _ => -1,
    };

    let mut cur_amount = i32::MAX;
    let mut ret = Vec::new();

    for &action in actions.iter().rev() {
        let amount = get_amount(action);
        if amount > 0 {
            let ratio = (amount - offset) as f64 / pot as f64;
            let cur_ratio = (cur_amount - offset) as f64 / pot as f64;
            let threshold_ratio = (cur_ratio - param) / (1.0 + param);
            if ratio < threshold_ratio * (1.0 - EPS) {
                ret.push(action);
                cur_amount = amount;
            }
        } else {
            ret.push(action);
        }
    }

    ret.reverse();
    ret
}
