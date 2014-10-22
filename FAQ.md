## Why not Vagrant?

The advantage over Vagrant is simply that it's written in Common Lisp and, as
such, available in Quicklisp without any external commands. This way, the
library can be included as a dependency and used without anyone having to set up
an external tool other than VirtualBox.

## Where are disk images stored?

Everything is stored in specific subdirectories under
`~/.config/corona/`. Vagrant Cloud images are stored, in their extracted form,
in `~/.config/corona/files/vagrant-cloud/`. The disk images of virtual machines
are stored in `~/.config/corona/files/virtual-machines`.

## How are VM names handled?

Virtual machines are identified by a name, which is a Common Lisp symbol. Inside
the VM directory, all the data for a virtual machine is stored inside a folder
for the package and another folder for the symbol name. For example:

```
virtual-machines/
  COMMON-LISP/
    TEST-VM/
    UBUNTU-PERSONAL/
  MY-APP/
    TESTING/
    STAGING/
```

The names of your virtual machines are restricted by the limitations of your
filesystem (Allowed characters, pathname length, etc.). Rather than add specific
checks for meaningless edge cases, I'll just warn you not to name your virtual
machines `myapp:My/Test\<<VM>>`.
