type StudPrivateStartingCards = [Vec<(Card, Card)>; 2];

type StudIndices = [Vec<u32>; 2];

#[derive(Debug, Clone)]
#[cfg_attr(feature = "bincode", derive(Decode, Encode))]
pub struct StudCardConfig {
    /// Initial range of each player.
    pub range: [Range; 2],

    pub third_street: (Card, Card),
    pub fourth_street: (Card, Card),
    pub fifth_street: (Card, Card),
    pub sixth_street: (Card, Card),
}

impl Default for StudCardConfig {
    #[inline]
    fn default() -> Self {
        Self {
            range: Default::default(),
            third_street: (NOT_DEALT, NOT_DEALT),
            fourth_street: (NOT_DEALT, NOT_DEALT),
            fifth_street: (NOT_DEALT, NOT_DEALT),
            sixth_street: (NOT_DEALT, NOT_DEALT),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct StudStrengthItem {
    pub(crate) strength: u32,
    pub(crate) index: u32,
}

impl StudCardConfig {
    pub(crate) fn valid_indices(
        &self,
        private_starting_cards: &StudPrivateStartingCards,
    ) -> (Vec<StudIndices>) {
        let board1 = self.third_street.0;
        let board2 = self.third_street.1;
        let board3 = self.fourth_street.0;
        let board4 = self.fourth_street.1;
        let board5 = self.fifth_street.0;
        let board6 = self.fifth_street.1;
        let board7 = self.sixth_street.0;
        let board8 = self.sixth_street.1;

        let mut ret = [
            Vec::with_capacity(private_starting_cards[0].len()),
            Vec::with_capacity(private_starting_cards[1].len()),
        ];

        let mut board_mask: u64 = 0;
        if board1 != NOT_DEALT {
            board_mask |= 1 << board1;
        }
        if board2 != NOT_DEALT {
            board_mask |= 1 << board2;
        }
        if board3 != NOT_DEALT {
            board_mask |= 1 << board3;
        }
        if board4 != NOT_DEALT {
            board_mask |= 1 << board4;
        }
        if board5 != NOT_DEALT {
            board_mask |= 1 << board5;
        }
        if board6 != NOT_DEALT {
            board_mask |= 1 << board6;
        }
        if board7 != NOT_DEALT {
            board_mask |= 1 << board7;
        }
        if board8 != NOT_DEALT {
            board_mask |= 1 << board8;
        }

        for player in 0..2 {
            ret[player].extend(
                private_starting_cards[player]
                    .iter()
                    .enumerate()
                    .filter_map(|(index, &(c1, c2))| {
                        let hand_mask: u64 = (1 << c1) | (1 << c2);
                        if hand_mask & board_mask == 0 {
                            Some(index as u32)
                        } else {
                            None
                        }
                    }),
            );

            ret[player].shrink_to_fit();
        }

        ret
    }

    pub(crate) fn hand_strength(
        &self,
        private_cards: &PrivateCards,
    ) -> Vec<[Vec<StrengthItem>; 2]> {
        let mut ret = vec![Default::default(); 52 * 51 / 2];

        let mut board = Hand::new();
        for &card in &self.flop {
            board = board.add_card(card as usize);
        }

        for board1 in 0..52 {
            for board2 in board1 + 1..52 {
                if !board.contains(board1 as usize)
                    && !board.contains(board2 as usize)
                    && (self.turn == NOT_DEALT || board1 == self.turn || board2 == self.turn)
                    && (self.river == NOT_DEALT || board1 == self.river || board2 == self.river)
                {
                    let board = board.add_card(board1 as usize).add_card(board2 as usize);
                    let mut strength = [
                        Vec::with_capacity(private_cards[0].len() + 2),
                        Vec::with_capacity(private_cards[1].len() + 2),
                    ];

                    for player in 0..2 {
                        // add the weakest and strongest sentinels
                        strength[player].push(StrengthItem {
                            strength: 0,
                            index: 0,
                        });
                        strength[player].push(StrengthItem {
                            strength: u16::MAX,
                            index: u16::MAX,
                        });

                        strength[player].extend(
                            private_cards[player].iter().enumerate().filter_map(
                                |(index, &(c1, c2))| {
                                    let (c1, c2) = (c1 as usize, c2 as usize);
                                    if board.contains(c1) || board.contains(c2) {
                                        None
                                    } else {
                                        let hand = board.add_card(c1).add_card(c2);
                                        Some(StrengthItem {
                                            strength: hand.evaluate() + 1, // +1 to avoid 0
                                            index: index as u16,
                                        })
                                    }
                                },
                            ),
                        );

                        strength[player].shrink_to_fit();
                        strength[player].sort_unstable();
                    }

                    ret[card_pair_to_index(board1, board2)] = strength;
                }
            }
        }

        ret
    }
}
