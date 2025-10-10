use bevy::ecs::system::In;

use crate::{CommandBuilder, CommandContext};

#[test]
#[expect(clippy::should_panic_without_expect)]
#[should_panic]
fn fail_invalid_command_name() {
    CommandBuilder::new("0%", None, |_: In<CommandContext>| unreachable!());
}
