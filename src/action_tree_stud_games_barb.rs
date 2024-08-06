use crate::card::*;
use crate::mutex_like::*;
use std::fmt;
use crate::range::*;

#[cfg(feature = "bincode")]
use bincode::{Decode, Encode};

pub(crate) const PLAYER_OOP: u8 = 0;
//pub(crate) const PLAYER_IP: u8 = 1;
//pub(crate) const PLAYER_CHANCE: u8 = 2; // only used with `PLAYER_CHANCE_FLAG`
//pub(crate) const PLAYER_MASK: u8 = 3;
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
    Bet(i32),

    /// Raise action
    Raise(i32),

    /// CR-someday: implement. All-in action with a specified amount.
    /// AllIn(i32),

    /// Chance action with a card ID, i.e., the dealing of a turn or river card.
    Chance((Card, Card)),
}

impl fmt::Display for StudAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            StudAction::None => write!(f, "None"),
            StudAction::Fold => write!(f, "Fold"),
            StudAction::Check => write!(f, "Check"),
            StudAction::Call => write!(f, "Call"),
            StudAction::Bet(amt) => write!(f, "Bet({})", amt),
            StudAction::Raise(amt) => write!(f, "Raise({})", amt),
            StudAction::Chance(cards) => write!(f, "Chance({})", hole_to_string(cards).unwrap()),
        }
    }
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
    /// Initial state of the game tree (which street)
    pub initial_state: StudBoardState,

    /// Starting pot size from antes. Must be greater than 0
    pub starting_pot_from_antes: i32,

    /// Initial stack, must be greater than 0. 
    pub effective_stack: i32,

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
pub struct StudActionTreeNode {
    player_whose_action: u8,
    board_state: StudBoardState,
    amount_wagered_so_far_this_game: i32,
    actions_so_far_this_game: Vec<StudAction>,
    children: Vec<MutexLike<StudActionTreeNode>>,
}

#[derive(Default, Clone)]
#[cfg_attr(feature = "bincode", derive(Decode, Encode))]
pub struct StudActionTreeNodeDebug {
    pub player_whose_action: u8,
    pub board_state: StudBoardState,
    pub amount_wagered_so_far_this_game: i32,
}

struct StudBuildTreeInfo {
    prev_action: StudAction,
    num_bets_on_this_street: i32,
    bet_size_on_this_street: i32,
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
    pub fn invalid_terminals(&self) -> Vec<Vec<StudAction>> {
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
    pub fn history(&self) -> &[StudAction] {
        &self.history
    }

    /// Applies the given action history from the root node.
    #[inline]
    pub fn apply_history(&mut self, history: &[StudAction]) -> Result<(), String> {
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
    pub fn available_actions(&self) -> &[StudAction] {
        &self.current_node_skip_chance().actions_so_far_this_game
    }

    /// Plays the given action. Returns `Ok(())` if the action is valid.
    ///
    /// The `action` must be one of the possible actions at the current node.
    /// If the current node is a chance node, the chance action is automatically played before
    /// playing the given action.
    #[inline]
    pub fn play(&mut self, action: StudAction) -> Result<(), String> {
        let node = self.current_node_skip_chance();
        if !node.actions_so_far_this_game.contains(&action) {
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

    /// Returns the reference to the current node.
    #[inline]
    pub fn current_node(&self) -> &StudActionTreeNode {
        unsafe {
            let mut node = &*self.root.lock() as *const StudActionTreeNode;
            for action in &self.history {
                while (*node).is_chance() {
                    node = &*(*node).children[0].lock();
                }
                let index = (*node).actions_so_far_this_game.iter().position(|x| x == action).unwrap();
                node = &*(*node).children[index].lock();
            }
            &*node
        }
    }

    #[inline]
    pub fn current_node_debug(&self) -> StudActionTreeNodeDebug {
        unsafe {
            let mut node = &*self.root.lock() as *const StudActionTreeNode;
            for action in &self.history {
                while (*node).is_chance() {
                    node = &*(*node).children[0].lock();
                }
                let index = (*node).actions_so_far_this_game.iter().position(|x| x == action).unwrap();
                node = &*(*node).children[index].lock();
            }

            let this_node = &*node;
            StudActionTreeNodeDebug{player_whose_action: this_node.player_whose_action, board_state: this_node.board_state, amount_wagered_so_far_this_game: this_node.amount_wagered_so_far_this_game}
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
        if config.starting_pot_from_antes <= 0 {
            return Err(format!(
                "Starting pot must be positive: {}",
                config.starting_pot_from_antes
            ));
        }
        Ok(())
    }

    #[inline]
    fn get_bet_size(node: &StudBoardState, curr_config: &StudTreeConfig) -> i32 {
        match node {
            StudBoardState::ThirdStreet | StudBoardState::FourthStreet => curr_config.small_bet_size,
            StudBoardState::FifthStreet | StudBoardState::SixthStreet | StudBoardState::SeventhStreet => curr_config.big_bet_size,
        }
    }

    /// Builds the action tree.
    #[inline]
    fn build_tree(&mut self) {
        
        let mut root = self.root.lock();
        let curr_config = self.config();
        *root = StudActionTreeNode::default();
        root.board_state = curr_config.initial_state;
        root.amount_wagered_so_far_this_game = curr_config.starting_pot_from_antes;

        // CR-someday: if we implement all ins make sure to pass in the new
        self.build_tree_recursive(&mut root, StudBuildTreeInfo::new(StudActionTree::get_bet_size(&curr_config.initial_state, curr_config)));

    }

    /// Recursively builds the action tree.
    fn build_tree_recursive(&self, node: &mut StudActionTreeNode, info: StudBuildTreeInfo) {
        let curr_config: &StudTreeConfig = self.config();

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
            let bet_size_on_this_street = StudActionTree::get_bet_size(&next_state, curr_config);

            // <TODO> calculate new player, but need the chance cards... maybe search through self.history
            // let next_player = StudActionTreeNode::calc_next_player(&node, )
            let _next_player = PLAYER_OOP;

            // Cr-someday: this is another spot to think about all-ins
            node.actions_so_far_this_game.push(StudAction::Chance((0,0)));
            
            //<TODO> why can we just push default values onto StudActionTreeNode for the last two variables???
            node.children.push(MutexLike::new(StudActionTreeNode {
                player_whose_action: PLAYER_OOP,
                board_state: next_state,
                amount_wagered_so_far_this_game: node.amount_wagered_so_far_this_game,
                ..Default::default()
            }));

            self.build_tree_recursive(
                &mut node.children[0].lock(),
                info.create_next(PLAYER_OOP, StudAction::Chance((0,0)), bet_size_on_this_street),
            );
        } else {
            self.push_actions(node, &info);
            let bet_size_on_this_street = StudActionTree::get_bet_size(&node.board_state, curr_config);
            for (action, child) in node.actions_so_far_this_game.iter().zip(node.children.iter()) {
                self.build_tree_recursive(
                    &mut child.lock(),
                    info.create_next(node.player_whose_action, *action, bet_size_on_this_street),
                );
            }
        }
    }

    /// Pushes all possible actions to the given node.
    fn push_actions(&self, node: &mut StudActionTreeNode, info: &StudBuildTreeInfo) {
        let mut actions = Vec::new();
        let player = node.player_whose_action;
        let opponent = node.player_whose_action ^ 1;
        let curr_config: &StudTreeConfig = self.config();

        match info.prev_action {
            StudAction::Chance(_) | StudAction::None => {
                actions.push(StudAction::Check);
                actions.push(StudAction::Bet(info.bet_size_on_this_street));
            }
            StudAction::Check => {
                actions.push(StudAction::Check);     
                actions.push(StudAction::Bet(info.bet_size_on_this_street));
            }
            StudAction::Bet(_) | StudAction::Raise(_) => {
                actions.push(StudAction::Fold);
                actions.push(StudAction::Call);

                if info.num_bets_on_this_street < curr_config.bets_cap { 
                    actions.push(StudAction::Raise(info.bet_size_on_this_street * (info.num_bets_on_this_street+1))); 
                }
            }
            StudAction::Call => {
            }
            StudAction::Fold => {
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
            let mut amount = node.amount_wagered_so_far_this_game;
            let next_player = match action {
                StudAction::Fold => PLAYER_FOLD_FLAG | player,
                StudAction::Check => player_after_check,
                StudAction::Call => {
                    amount += info.bet_size_on_this_street;
                    player_after_call
                }
                StudAction::Bet(_) => {
                    amount += info.bet_size_on_this_street;
                    opponent
                }
                StudAction::Raise(_) => {
                    amount += 2*info.bet_size_on_this_street;
                    opponent
                }
                _ => panic!("Unexpected action: {action:?}"),
            };

            node.actions_so_far_this_game.push(action);
            node.children.push(MutexLike::new(StudActionTreeNode {
                player_whose_action: next_player,
                board_state: node.board_state,
                amount_wagered_so_far_this_game: amount,
                ..Default::default()
            }));
        }

        node.actions_so_far_this_game.shrink_to_fit();
        node.children.shrink_to_fit();
    }

    /// Recursive function to enumerate all invalid terminal nodes.
    fn invalid_terminals_recursive(
        node: &StudActionTreeNode,
        result: &mut Vec<Vec<StudAction>>,
        line: &mut Vec<StudAction>,
    ) {
        if node.is_terminal() {
            // do nothing
        } else if node.children.is_empty() {
            result.push(line.clone());
        } else if node.is_chance() {
            Self::invalid_terminals_recursive(&node.children[0].lock(), result, line)
        } else {
            for (&action, child) in node.actions_so_far_this_game.iter().zip(node.children.iter()) {
                line.push(action);
                Self::invalid_terminals_recursive(&child.lock(), result, line);
                line.pop();
            }
        }
    }
   
}

impl StudActionTreeNode {
    #[inline]
    fn is_terminal(&self) -> bool {
        self.player_whose_action & PLAYER_TERMINAL_FLAG != 0
    }

    #[inline]
    fn is_chance(&self) -> bool {
        self.player_whose_action & PLAYER_CHANCE_FLAG != 0
    }
}

impl StudBuildTreeInfo {
    #[inline]
    fn new(bet_size: i32) -> Self {
        Self {
            prev_action: StudAction::None,
            num_bets_on_this_street: 0,
            bet_size_on_this_street:bet_size,
        }
    }

    #[inline]
    // Don't really need player for now if we don't have all-in considerations
    fn create_next(&self, _player: u8, action: StudAction, bet_size_on_this_street: i32) -> Self {
        let mut num_bets_on_this_street = self.num_bets_on_this_street;
        
        match action {
            StudAction::Check => {
            }
            StudAction::Call => {
            }
            StudAction::Bet(_) => {
                num_bets_on_this_street += 1;
            }
            StudAction::Raise(_) => {
                num_bets_on_this_street += 1;
            }
            _ => {}
        }

        // bet_size_on_this_street only encodes whether we are in small or big bet world
        // the actual bet size can be considered num_bets_on_this_street * bet_size_on_this_street
        StudBuildTreeInfo {
            prev_action: action,
            num_bets_on_this_street,
            bet_size_on_this_street, 
        }
    }
}

/// Returns the number of action nodes of [flop, turn, river].
#[allow(dead_code)]
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

#[allow(dead_code)]
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
