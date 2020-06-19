# CMachine
Implementation of CMachine in OCaml. CMachine is introduced at the book "Compiler Design - Virtual Machines", Reinhard Wilhelm, Helmut Seidl wrote.

## Dependencies
* opam (>= 2.0.4)
  * OCaml (>= 4.08.1)
  * dune

## Featured Types and Values
| Type, and Value           | Description |
| ------------------------- | ----------- |
| `Lang.inst`               | Definition of the CMachine Instructions |
| `Lang.stack`              | Definition of the CMachine Data-Memory |
| `Lang.store`              | Definition of the CMachine Instruction-Memory |
| `Lang.runtimeErr`         | Definition of the Errors might be occured while the CMachine runs |
| `Machine.state`           | Definition of the single state inside CMachine |
| `Machine.stackSize`       | The size of the Data-Memory |
| `Machine.emptyState`      | Convenient placeholder to make a state value |
| `Machine.copyState`       | Deep-copy function for the state value. Plain let-in binding for copying state might cause some unwanted side-effects. |
| `Machine.execute`         | One-step execution for the given instruction and the Machine state |
| `Machine.execute_one`     | One-step execution for the given Machine state |
| `Machine.run`             | Full-execution for the given Machine state |
