## Defining Machines

Machines are defined with the `defmachine` macro. The first argument to
`defmachine` is a symbol, which will be the machine's name (Including the
package). The other arguments are:

`system`
: This is a system triple, a literal list with three elements which uniquely
identifies a base system: The system's name, version, and architecture.

`memory`
: The amount of RAM given to the VM in megabytes. Default: `512`.

`cpu-count`
: The number of virtual CPUs. One by default.

`ip`
: The system's IP address. By default this is not used.

## Controlling VM State

The following six functions can be used to control the state of the virtual
machines:

`start`, `stop`
: Start and shut down the VM. If possible, shut it down gently.

`pause`, `resume`
: Pause and resume the VM.

`reboot`
: Reboot the virtual machine.

`poweroff`
: Force VM shutdown.
