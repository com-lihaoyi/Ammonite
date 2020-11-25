package ammonite.repl.api;

import ammonite.compiler.iface.Frame;

public abstract class Session {
  /**
    * The current stack of frames
    */
  public abstract Frame[] frames();

  /**
    * Checkpoints your current work, placing all future work into its own
    * frames. If a name is provided, it can be used to quickly recover
    * that checkpoint later.
    */
  public abstract void save(String name);

  public final void save() {
    save("");
  }

  /**
    * Discards the last frames, effectively reverting your session to
    * the last `save`-ed checkpoint. If a name is provided, it instead reverts
    * your session to the checkpoint with that name.
    */
  public abstract SessionChanged load(String name);

  public final SessionChanged load() {
    return load("");
  }

  /**
    * Resets you to the last save point. If you pass in `num`, it resets
    * you to that many savepoints since the last one.
    */
  public abstract SessionChanged pop(int num);

  public final SessionChanged pop() {
    return pop(1);
  }

  /**
    * Deletes a named checkpoint, allowing it to be garbage collected if it
    * is no longer accessible.
    */
  public abstract void delete(String name);
}
