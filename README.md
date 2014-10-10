# Corona

Corona is a library for creating and managing virtual machines from Common
Lisp. All you need is VirtualBox and an internet connection.

Corona uses [Vagrant Cloud][vc] as a source of base systems to bootstrap virtual
machines from.

[vc]: https://vagrantcloud.com/

## Usage

~~~lisp
(defmachine my-app
  :system (:ubuntu :14.04 :64)
  :memory 1024)

(start my-app)

(stop my-app)
~~~

That's it.

# Use Cases

## Development Environments

Corona can be used to create isolated, reproducible development environments so
you and your team can work on the same system.

No more 'works on my machine', no more difference between development and
production.

## Testing

If you have a library that uses an external tool, like a database server or
something equally large, you can use Corona to set up a virtual machine and
install whatever dependencies you need, so the user doesn't actually have to run
anything on their computer.

Additionally, since you can set up multiple virtual machines with different
systems, you can use Corona to ensure your library works on most operating
systems. This is especially useful for testing compilers and similar
applications where portability is critical.

## Building

You can use Corona as a build server: Fire up virtual machines of the operating
system you want to build on, set them up with everything you need, and run the
builds.

# FAQ

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

~~~
virtual-machines/
  COMMON-LISP/
    TEST-VM/
    UBUNTU-PERSONAL/
  MY-APP/
    TESTING/
    STAGING/
~~~

The names of your virtual machines are restricted by the limitations of your
filesystem (Allowed characters, pathname length, etc.). Rather than add specific
checks for meaningless edge cases, I'll just warn you not to name your virtual
machines `myapp:My/Test\<<VM>>`.

# License

Copyright (c) 2014 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.
