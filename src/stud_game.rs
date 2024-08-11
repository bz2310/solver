use crate::interface::*;
use crate::mutex_like::*;
use std::mem::{self, MaybeUninit};

use crate::action_tree_stud_games_barb::*;
use crate::card::*;
use crate::range::*;
use crate::utility::*;

use std::collections::BTreeMap;
use std::ptr;
use std::slice;

#[cfg(feature = "bincode")]
use bincode::{Decode, Encode};
#[cfg(feature = "rayon")]
use rayon::prelude::*;

#[derive(Default)]
struct BuildTreeInfo {
    third_street_index: (usize, usize),
    fourth_street_index: (usize, usize),
    fifth_street_index: (usize, usize),
    sixth_street_index: (usize, usize),
    seventh_street_index: (usize, usize),
    num_storage: u64,
    num_storage_ip: u64,
    num_storage_chance: u64,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct StudNode {
    prev_action: StudAction,
    player: u8,
    third_street: (Card, Card),
    fourth_street: (Card, Card),
    fifth_street: (Card, Card),
    sixth_street: (Card, Card),
    seventh_street: (Card, Card),
    is_locked: bool,
    amount: i32,
    children_offset: u32,
    num_children: u16,
    num_elements_ip: u16,
    num_elements: u32,
    scale1: f32,
    scale2: f32,
    scale3: f32,
    storage1: *mut u8, // strategy
    storage2: *mut u8, // regrets or cfvalues
    storage3: *mut u8, // IP cfvalues
}
unsafe impl Send for StudNode {}
unsafe impl Sync for StudNode {}

impl GameNode for StudNode {
    #[inline]
    fn is_terminal(&self) -> bool {
        self.player & PLAYER_TERMINAL_FLAG != 0
    }

    #[inline]
    fn is_chance(&self) -> bool {
        self.player & PLAYER_CHANCE_FLAG != 0
    }

    #[inline]
    fn cfvalue_storage_player(&self) -> Option<usize> {
        let prev_player = self.player & PLAYER_MASK;
        match prev_player {
            0 => Some(1),
            1 => Some(0),
            _ => None,
        }
    }

    #[inline]
    fn player(&self) -> usize {
        self.player as usize
    }

    #[inline]
    fn num_actions(&self) -> usize {
        self.num_children as usize
    }

    #[inline]
    fn play(&self, action: usize) -> MutexGuardLike<Self> {
        self.children()[action].lock()
    }

    #[inline]
    fn strategy(&self) -> &[f32] {
        unsafe { slice::from_raw_parts(self.storage1 as *const f32, self.num_elements as usize) }
    }

    #[inline]
    fn strategy_mut(&mut self) -> &mut [f32] {
        unsafe { slice::from_raw_parts_mut(self.storage1 as *mut f32, self.num_elements as usize) }
    }

    #[inline]
    fn regrets(&self) -> &[f32] {
        unsafe { slice::from_raw_parts(self.storage2 as *const f32, self.num_elements as usize) }
    }

    #[inline]
    fn regrets_mut(&mut self) -> &mut [f32] {
        unsafe { slice::from_raw_parts_mut(self.storage2 as *mut f32, self.num_elements as usize) }
    }

    #[inline]
    fn cfvalues(&self) -> &[f32] {
        unsafe { slice::from_raw_parts(self.storage2 as *const f32, self.num_elements as usize) }
    }

    #[inline]
    fn cfvalues_mut(&mut self) -> &mut [f32] {
        unsafe { slice::from_raw_parts_mut(self.storage2 as *mut f32, self.num_elements as usize) }
    }

    #[inline]
    fn has_cfvalues_ip(&self) -> bool {
        self.num_elements_ip != 0
    }

    #[inline]
    fn cfvalues_ip(&self) -> &[f32] {
        unsafe { slice::from_raw_parts(self.storage3 as *const f32, self.num_elements_ip as usize) }
    }

    #[inline]
    fn cfvalues_ip_mut(&mut self) -> &mut [f32] {
        unsafe {
            slice::from_raw_parts_mut(self.storage3 as *mut f32, self.num_elements_ip as usize)
        }
    }

    #[inline]
    fn cfvalues_chance(&self) -> &[f32] {
        unsafe { slice::from_raw_parts(self.storage1 as *const f32, self.num_elements as usize) }
    }

    #[inline]
    fn cfvalues_chance_mut(&mut self) -> &mut [f32] {
        unsafe { slice::from_raw_parts_mut(self.storage1 as *mut f32, self.num_elements as usize) }
    }

    #[inline]
    fn strategy_compressed(&self) -> &[u16] {
        unsafe { slice::from_raw_parts(self.storage1 as *const u16, self.num_elements as usize) }
    }

    #[inline]
    fn strategy_compressed_mut(&mut self) -> &mut [u16] {
        unsafe { slice::from_raw_parts_mut(self.storage1 as *mut u16, self.num_elements as usize) }
    }

    #[inline]
    fn regrets_compressed(&self) -> &[i16] {
        unsafe { slice::from_raw_parts(self.storage2 as *const i16, self.num_elements as usize) }
    }

    #[inline]
    fn regrets_compressed_mut(&mut self) -> &mut [i16] {
        unsafe { slice::from_raw_parts_mut(self.storage2 as *mut i16, self.num_elements as usize) }
    }

    #[inline]
    fn cfvalues_compressed(&self) -> &[i16] {
        unsafe { slice::from_raw_parts(self.storage2 as *const i16, self.num_elements as usize) }
    }

    #[inline]
    fn cfvalues_compressed_mut(&mut self) -> &mut [i16] {
        unsafe { slice::from_raw_parts_mut(self.storage2 as *mut i16, self.num_elements as usize) }
    }

    #[inline]
    fn cfvalues_ip_compressed(&self) -> &[i16] {
        unsafe { slice::from_raw_parts(self.storage3 as *const i16, self.num_elements_ip as usize) }
    }

    #[inline]
    fn cfvalues_ip_compressed_mut(&mut self) -> &mut [i16] {
        unsafe {
            slice::from_raw_parts_mut(self.storage3 as *mut i16, self.num_elements_ip as usize)
        }
    }

    #[inline]
    fn cfvalues_chance_compressed(&self) -> &[i16] {
        unsafe { slice::from_raw_parts(self.storage1 as *const i16, self.num_elements as usize) }
    }

    #[inline]
    fn cfvalues_chance_compressed_mut(&mut self) -> &mut [i16] {
        unsafe { slice::from_raw_parts_mut(self.storage1 as *mut i16, self.num_elements as usize) }
    }

    #[inline]
    fn strategy_scale(&self) -> f32 {
        self.scale1
    }

    #[inline]
    fn set_strategy_scale(&mut self, scale: f32) {
        self.scale1 = scale;
    }

    #[inline]
    fn regret_scale(&self) -> f32 {
        self.scale2
    }

    #[inline]
    fn set_regret_scale(&mut self, scale: f32) {
        self.scale2 = scale;
    }

    #[inline]
    fn cfvalue_scale(&self) -> f32 {
        self.scale2
    }

    #[inline]
    fn set_cfvalue_scale(&mut self, scale: f32) {
        self.scale2 = scale;
    }

    #[inline]
    fn cfvalue_ip_scale(&self) -> f32 {
        self.scale3
    }

    #[inline]
    fn set_cfvalue_ip_scale(&mut self, scale: f32) {
        self.scale3 = scale;
    }

    #[inline]
    fn cfvalue_chance_scale(&self) -> f32 {
        self.scale1
    }

    #[inline]
    fn set_cfvalue_chance_scale(&mut self, scale: f32) {
        self.scale1 = scale;
    }

    #[inline]
    fn enable_parallelization(&self) -> bool {
        self.seventh_street == (NOT_DEALT, NOT_DEALT)
    }
}

impl Default for StudNode {
    #[inline]
    fn default() -> Self {
        Self {
            prev_action: StudAction::None,
            player: PLAYER_OOP,
            third_street: (NOT_DEALT, NOT_DEALT),
            fourth_street: (NOT_DEALT, NOT_DEALT),
            fifth_street: (NOT_DEALT, NOT_DEALT),
            sixth_street: (NOT_DEALT, NOT_DEALT),
            seventh_street: (NOT_DEALT, NOT_DEALT),
            is_locked: false,
            amount: 0,
            children_offset: 0,
            num_children: 0,
            num_elements_ip: 0,
            storage1: ptr::null_mut(),
            storage2: ptr::null_mut(),
            storage3: ptr::null_mut(),
            num_elements: 0,
            scale1: 0.0,
            scale2: 0.0,
            scale3: 0.0,
        }
    }
}

impl StudNode {
    #[inline]
    pub(super) fn children(&self) -> &[MutexLike<Self>] {
        // This is safe because `MutexLike<T>` is a `repr(transparent)` wrapper around `T`.
        let self_ptr = self as *const _ as *const MutexLike<StudNode>;
        unsafe {
            slice::from_raw_parts(
                self_ptr.add(self.children_offset as usize),
                self.num_children as usize,
            )
        }
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
#[cfg_attr(feature = "bincode", derive(Decode, Encode))]
enum State {
    ConfigError = 0,
    #[default]
    Uninitialized = 1,
    TreeBuilt = 2,
    MemoryAllocated = 3,
    Solved = 4,
}

/// A struct representing a postflop game.
#[derive(Default)]
pub struct StudGame {
    // state
    state: State,

    // postflop game configurations
    card_config: CardConfig,
    tree_config: StudTreeConfig,
    added_lines: Vec<Vec<StudAction>>,
    removed_lines: Vec<Vec<StudAction>>,
    action_root: Box<MutexLike<StudActionTreeNode>>,

    // computed from configurations
    num_combinations: f64,
    initial_weights: [Vec<f32>; 2],
    private_starting_cards: [Vec<(Card, Card)>; 2],
    private_seventh_street_cards: [Vec<Card>; 2],
    same_hand_index: [Vec<u16>; 2],

    // indices in `private_starting_cards` that do not conflict with the specified board cards
    valid_indices_third: [Vec<u16>; 2],
    valid_indices_fourth: Vec<[Vec<u16>; 2]>,
    valid_indices_fifth: Vec<[Vec<u16>; 2]>,
    valid_indices_sixth: Vec<[Vec<u16>; 2]>,
    // valid_indices_seventh: Vec<[Vec<u16>; 2]>,

    // indices in private_seventh_street_cards that do not conflict with board
    // valid_indices_seventh:

    // hand strength information: indices are stored in ascending strength order
    hand_strength: Vec<[Vec<StrengthItem>; 2]>,

    // isomorphism information
    // - `isomorphism_ref_*`: indices to which the eliminated events should refer
    // - `isomorphism_card_*`: list of cards eliminated by the isomorphism
    // - `isomorphism_swap_*`: list of hand index pairs that should be swapped when applying the
    // //                         isomorphism with the specified suit
    // isomorphism_ref_turn: Vec<u8>,
    // isomorphism_card_turn: Vec<Card>,
    // isomorphism_swap_turn: [SwapList; 4],
    // isomorphism_ref_river: Vec<Vec<u8>>,
    // isomorphism_card_river: [Vec<Card>; 4],
    // isomorphism_swap_river: [[SwapList; 4]; 4],

    // store options
    storage_mode: StudBoardState,
    target_storage_mode: StudBoardState,
    num_nodes: [u64; 3],
    is_compression_enabled: bool,
    num_storage: u64,
    num_storage_ip: u64,
    num_storage_chance: u64,
    misc_memory_usage: u64,

    // global storage
    // `storage*` are used as a global storage and are referenced by `PostFlopNode::storage*`.
    // Methods like `PostFlopNode::strategy` define how the storage is used.
    node_arena: Vec<MutexLike<StudNode>>,
    storage1: Vec<u8>,
    storage2: Vec<u8>,
    storage_ip: Vec<u8>,
    storage_chance: Vec<u8>,
    locking_strategy: BTreeMap<usize, Vec<f32>>,

    // result interpreter
    action_history: Vec<usize>,
    node_history: Vec<usize>,
    is_normalized_weight_cached: bool,
    third_street: (Card, Card),
    fourth_street: (Card, Card),
    fifth_street: (Card, Card),
    sixtth_street: (Card, Card),
    seventh_street: (Card, Card),
    // turn_swapped_suit: Option<(u8, u8)>,
    // turn_swap: Option<u8>,
    // river_swap: Option<(u8, u8)>,
    total_bet_amount: [i32; 2],
    weights: [Vec<f32>; 2],
    normalized_weights: [Vec<f32>; 2],
    cfvalues_cache: [Vec<f32>; 2],
}

impl StudGame {
    pub(super) fn evaluate_internal(
        &self,
        result: &mut [MaybeUninit<f32>],
        node: &StudNode,
        player: usize,
        cfreach: &[f32],
    ) {
        let pot = (self.tree_config.starting_pot_from_antes + 2 * node.amount) as f64;
        let half_pot = 0.5 * pot;
        let amount_win = (half_pot) / self.num_combinations;
        let amount_lose = -half_pot / self.num_combinations;

        let player_starting_cards = &self.private_starting_cards[player];
        let opponent_starting_cards = &self.private_starting_cards[player ^ 1];

        let mut cfreach_sum = 0.0;
        let mut cfreach_minus = [0.0; 52];

        result.iter_mut().for_each(|v| {
            v.write(0.0);
        });

        let result = unsafe { &mut *(result as *mut _ as *mut [f32]) };

        // someone folded
        if node.player & PLAYER_FOLD_FLAG == PLAYER_FOLD_FLAG {
            let folded_player = node.player & PLAYER_MASK;
            let payoff = if folded_player as usize != player {
                amount_win
            } else {
                amount_lose
            };

            let valid_indices = StudCardConfig::valid_indices(&self, self.private_starting_cards);

            let opponent_indices = &valid_indices[player ^ 1];
            for &i in opponent_indices {
                unsafe {
                    let cfreach_i = *cfreach.get_unchecked(i as usize);
                    if cfreach_i != 0.0 {
                        let (c1, c2) = *opponent_cards.get_unchecked(i as usize);
                        let cfreach_i_f64 = cfreach_i as f64;
                        cfreach_sum += cfreach_i_f64;
                        *cfreach_minus.get_unchecked_mut(c1 as usize) += cfreach_i_f64;
                        *cfreach_minus.get_unchecked_mut(c2 as usize) += cfreach_i_f64;
                    }
                }
            }

            if cfreach_sum == 0.0 {
                return;
            }

            let player_indices = &valid_indices[player];
            let same_hand_index = &self.same_hand_index[player];
            for &i in player_indices {
                unsafe {
                    let (c1, c2) = *player_cards.get_unchecked(i as usize);
                    let same_i = *same_hand_index.get_unchecked(i as usize);
                    let cfreach_same = if same_i == u16::MAX {
                        0.0
                    } else {
                        *cfreach.get_unchecked(same_i as usize) as f64
                    };
                    // inclusion-exclusion principle
                    let cfreach = cfreach_sum + cfreach_same
                        - *cfreach_minus.get_unchecked(c1 as usize)
                        - *cfreach_minus.get_unchecked(c2 as usize);
                    *result.get_unchecked_mut(i as usize) = (payoff * cfreach) as f32;
                }
            }
        }
        // showdown (optimized for no rake; 2-pass)
        // else if rake == 0.0 {
        let pair_index = card_pair_to_index(node.turn, node.river);
        let hand_strength = &self.hand_strength[pair_index];
        let player_strength = &hand_strength[player];
        let opponent_strength = &hand_strength[player ^ 1];

        let valid_player_strength = &player_strength[1..player_strength.len() - 1];
        let mut i = 1;

        for &StrengthItem { strength, index } in valid_player_strength {
            unsafe {
                while opponent_strength.get_unchecked(i).strength < strength {
                    let opponent_index = opponent_strength.get_unchecked(i).index as usize;
                    let cfreach_i = *cfreach.get_unchecked(opponent_index);
                    if cfreach_i != 0.0 {
                        let (c1, c2) = *opponent_cards.get_unchecked(opponent_index);
                        let cfreach_i_f64 = cfreach_i as f64;
                        cfreach_sum += cfreach_i_f64;
                        *cfreach_minus.get_unchecked_mut(c1 as usize) += cfreach_i_f64;
                        *cfreach_minus.get_unchecked_mut(c2 as usize) += cfreach_i_f64;
                    }
                    i += 1;
                }
                let (c1, c2) = *player_cards.get_unchecked(index as usize);
                let cfreach = cfreach_sum
                    - cfreach_minus.get_unchecked(c1 as usize)
                    - cfreach_minus.get_unchecked(c2 as usize);
                *result.get_unchecked_mut(index as usize) = (amount_win * cfreach) as f32;
            }
        }

        cfreach_sum = 0.0;
        cfreach_minus.fill(0.0);
        i = opponent_strength.len() - 2;

        for &StrengthItem { strength, index } in valid_player_strength.iter().rev() {
            unsafe {
                while opponent_strength.get_unchecked(i).strength > strength {
                    let opponent_index = opponent_strength.get_unchecked(i).index as usize;
                    let cfreach_i = *cfreach.get_unchecked(opponent_index);
                    if cfreach_i != 0.0 {
                        let (c1, c2) = *opponent_cards.get_unchecked(opponent_index);
                        let cfreach_i_f64 = cfreach_i as f64;
                        cfreach_sum += cfreach_i_f64;
                        *cfreach_minus.get_unchecked_mut(c1 as usize) += cfreach_i_f64;
                        *cfreach_minus.get_unchecked_mut(c2 as usize) += cfreach_i_f64;
                    }
                    i -= 1;
                }
                let (c1, c2) = *player_cards.get_unchecked(index as usize);
                let cfreach = cfreach_sum
                    - cfreach_minus.get_unchecked(c1 as usize)
                    - cfreach_minus.get_unchecked(c2 as usize);
                *result.get_unchecked_mut(index as usize) += (amount_lose * cfreach) as f32;
            }
        }
        // }
    }
}

impl Game for StudGame {
    type Node = StudNode;

    #[inline]
    fn root(&self) -> MutexGuardLike<Self::Node> {
        self.node_arena[0].lock()
    }

    #[inline]
    fn num_private_hands(&self, player: usize) -> usize {
        self.private_cards[player].len()
    }

    #[inline]
    fn initial_weights(&self, player: usize) -> &[f32] {
        &self.initial_weights[player]
    }

    #[inline]
    fn evaluate(
        &self,
        result: &mut [MaybeUninit<f32>],
        node: &Self::Node,
        player: usize,
        cfreach: &[f32],
    ) {
        self.evaluate_internal(result, node, player, cfreach);
    }

    #[inline]
    fn chance_factor(&self, node: &Self::Node) -> usize {
        if node.fourth_street == (NOT_DEALT, NOT_DEALT) {
            46
        } else if node.fifth_street == (NOT_DEALT, NOT_DEALT) {
            44
        } else if node.sixth_street == (NOT_DEALT, NOT_DEALT) {
            42
        } else {
            40
        }
    }

    #[inline]
    fn is_solved(&self) -> bool {
        self.state == State::Solved
    }

    #[inline]
    fn set_solved(&mut self) {
        self.state = State::Solved;
        let history = self.action_history.clone();
        self.apply_history(&history);
    }

    #[inline]
    fn is_ready(&self) -> bool {
        self.state == State::MemoryAllocated && self.storage_mode == StudBoardState::SeventhStreet
    }

    #[inline]
    fn is_raked(&self) -> bool {
        false
        // self.tree_config.rake_rate > 0.0 && self.tree_config.rake_cap > 0.0
    }

    #[inline]
    fn isomorphic_chances(&self, node: &Self::Node) -> &[u8] {
        if node.turn == NOT_DEALT {
            &self.isomorphism_ref_turn
        } else {
            &self.isomorphism_ref_river[node.turn as usize]
        }
    }

    #[inline]
    fn isomorphic_swap(&self, node: &Self::Node, index: usize) -> &[Vec<(u16, u16)>; 2] {
        if node.turn == NOT_DEALT {
            &self.isomorphism_swap_turn[self.isomorphism_card_turn[index] as usize & 3]
        } else {
            &self.isomorphism_swap_river[node.turn as usize & 3]
                [self.isomorphism_card_river[node.turn as usize & 3][index] as usize & 3]
        }
    }

    #[inline]
    fn locking_strategy(&self, node: &Self::Node) -> &[f32] {
        if !node.is_locked {
            &[]
        } else {
            let index = self.node_index(node);
            self.locking_strategy.get(&index).unwrap()
        }
    }

    #[inline]
    fn is_compression_enabled(&self) -> bool {
        self.is_compression_enabled
    }
}
