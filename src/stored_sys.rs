//! Provides a structure for storing type-erased systems to be registered to an [`App`].

use bevy::{
    app::App,
    ecs::system::{IntoSystem, SystemId, SystemInput},
};

/// A closure that registers a system to an app, and returns the system ID of that system.
type Registrar<I, O> = dyn FnOnce(&mut App) -> SystemId<I, O> + Send + Sync;

/// A stored system with the input `I` and the output `O`. This may then be registered with `StoredSystem::register`.
pub struct StoredSystem<I, O = ()>
where
    I: SystemInput + 'static,
    O: 'static,
{
    /// The registrar. View [`Registrar`] for more information.
    registrar: Box<Registrar<I, O>>,
}

impl<I, O> StoredSystem<I, O>
where
    I: SystemInput + 'static,
    O: 'static,
{
    /// Create a stored system from the given system.
    pub fn new<S, M>(system: S) -> Self
    where
        S: IntoSystem<I, O, M> + Send + Sync + 'static,
    {
        let registrar =
            Box::new(move |app: &mut App| -> SystemId<I, O> { app.register_system(system) });
        Self { registrar }
    }

    /// Register this system to the [`App`].
    pub fn register(self, app: &mut App) -> SystemId<I, O> {
        (self.registrar)(app)
    }
}
