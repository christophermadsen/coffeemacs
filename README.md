# Coffeemacs - A loosely maintained Emacs data science config for Windows.
I worked at a company where the entire data science team used an Emacs
configuration on Unix based systems. The config was maintained mainly by a few
guys. This is my attempt at bringing the parts I liked the best over to a
config working on Windows machines. Why? You may ask. Well, simply because if I
am gaming or browsing on my Windows PC at home, I don't want to switch systems
if I want to write down a note in org-roam or write some quick code.

<p align="center">
    <img src="coffee-isometric-dashboard.png">
</p>

## Make it work
### Add a HOME to the windows system environment variables
THe files in the `coffeemacs` repository should be added to your .emacs.d
directory. The .emacs.d directory is by default located where you install
emacs, but if we add a HOME variable to the Windows system environment
variables, then emacs will read the .emacs.d at that path. This is a lot more
handy and simulates a little bit how Linux paths work. Other practical things
to include at this destination could be your `.gitconfig` or `.ssh/`.

1. Open the Windows Start menu and search for 'Edit the system environment
   variable', a window should pop open when you click the correct result.
2. Click the `Environment Variables...` button in this new window. A new window
   should pop open again.
3. In this 2nd new window, under "System variables" (bottom part) click the
   `New` button.
4. Use the variable name `HOME` and set the variable value to whereever you
   want. I recommend using the `C:/Users/Name-of-your-user` path.
   
You will see that Windows recognises the `~/` path as your newly set `HOME`
system environment variable.

### Modify the paths in `personal.el`
Open the `personal.el` file in the base of coffeemacs directory. There you will
see the following:

``` emacs-lisp
;; Set name and email.
(setq user-full-name "Your Name Here"
      user-mail-address "")

;; Start dired in a specific folder
(setq default-directory "Path to your home/working directory")
```

Edit the values to something that fits you. For reference I use the following:
``` emacs-lisp
(setq user-full-name "Christopher Buch Madsen"
      user-mail-address "")
      
(setq default-directory "C:/Users/Chris")
```

You can also edit the values of the other variables you'll find there now, if
you want to use their functionalities, but more is explained below.

### Install Python (3.11 and 3.12 have been tested and works)
Make sure to add python to **PATH** when prompted during installation.

### Python Packages required to run coffeemacs.
(You'll also find these in a requirements.txt to install with `pip -r`)
```
setuptools
black
black-macchiato
isort
jupyter
flake8
ipykernel
virtualenvwrapper
virtualenvwrapper-win
```

### Flycheck
For the flycheck package to work you may need to go to "Manage App Execution
Aliases" in Windows and turn off "App Installer python.exe" and "App installer
python3.exe"

### Markdown (and other files to convert from/to)
Org-mode is amazing, but alternatives are sometimes used for specific purposes,
such as markdown for READMEs on gitlab and github (org files can work). To
enable all the functionalities behind the conversion and preview of these
things in coffeemacs, we need Pandoc. Install the latest version at:
https://pandoc.org/installing.html

Depending on where you install it, you may need to edit the following line inside coffeemacs:
``` emacs-lisp
(custom-set-variables
 '(markdown-command "C:/Pandoc/"))
```

### Installing a new python kernel for use in org-mode
In any terminal:
1. `mkvirtualenv "your kernel name"`
2. `python -m ipykernel install --name="your kernel name"`

Evaluate this within Emacs:
3.
``` emacs-lisp
(my/jupyter-refresh-kernelspecs)
```

### Use the (bash) workon command for activating a venv within PowerShell/Terminal:
find you PowerShell profile path by entering `$profile` in a PowerShell/Terminal,
then open that file and add (https://stackoverflow.com/a/47798786):
``` powershell
function workon ($env) {
        & $env:WORKON_HOME\$env\Scripts\activate.ps1
}
```

### Using Org-roam (SQLITE)
To use org-roam you need SQLITE, the easiest way is to install that via
`chocolatey`, a package manager for Windows.

We need to be able to run scripts from PowerShell/Terminal, so open an instance
of PowerShell as admin and run:
``` powershell
Set-ExecutionPolicy Bypass -Scope Process
```
Agree [Y] when prompted by the terminal.

Next, run the following to install Chocolatey:
``` powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1')) 
```

You may need to close and reopen PowerShell to then use chocolatey, if it
works, great success! If not, have a look at this link:
https://www.liquidweb.com/kb/how-to-install-chocolatey-on-windows/

We can then get SQLITE with chocolatey from a PowerShell/Terminal with admin.
``` powershell
choco install sqlite
```
Agree [Y] when prompted by the terminal.

If all else fails, try and have a look at:
https://github.com/nobiot/Zero-to-Emacs-and-Org-roam/tree/main and
https://lucidmanager.org/productivity/taking-notes-with-emacs-org-mode-and-org-roam/
maybe that will help.

### The modeline symbols are broken
This is because DOOM modeline is missing a dependency, the nerd fonts icons. We
can get this by manually running `M-x nerd-icons-install-fonts` you will then
be prompted for a place to install them, choose any desired
location. Afterwards, move the installed font file(s) to the Fonts folder in
your Windows install, mine is at `C:/Windows/Fonts/`. You will need to restart
Emacs for this to take effect.

### Org-roam Graph
If we want to generate a graph with `M-x org-roam-graph` we need to first
install an executable to perform this. Graphviz is mentioned in the org-roam
manual, so we'll simply use that too
(https://www.orgroam.com/manual.html#org_002droam_002dgraph). Download and
install Graphviz at https://graphviz.org/download/#windows (don't forget to add
it to **PATH** during installation), and in the personal.el file you can then
change the path to the executable.

``` emacs-lisp
;; Path to Graphviz executable for org-roam visualizer
(setq org-roam-graph-executable "C:/Graphviz/bin/dot.exe")
```

You can also choose which program to execute the visualization with, I recommend using your browser, for example Chrome.

``` emacs-lisp
;; Path to executable for viewing the org-roam visualization
(setq org-roam-graph-viewer "")
```

### Org-roam bug (As of 8/8/2023)
https://github.com/org-roam/org-roam/issues/2361
There is a bug with org-roam which makes it not able to read files with links.
Roam notes pretty much all contain links to one another, so this sucks.
It can be fixed by using an older version of org. We can fetch this by defining
which version to use in a straight.el 'lockfile', which should be placed in
`/straight/versions/`. In coffeemacs.el the bugfix is handled automatically,
but don't forget to come back and check up on the bug to see if it's fixed.
(bugfix.el will be copied to `/straight/versions/default.el` during load time).

### py-isort
Within py-isort there's a function call to "diff", this doesn't exist in
Windows and we'll instead use the Windows equivalent "FC". We need to
change this in the py-isort.el package, otherwise it likely won't work. It
should exist within ".emacs.d/elpa/py-isort". Replace "diff" with "FC" in the line calling it.

(I have provided a py-isort.el in lisp/ that fixes this, but it'd be best to
not have a separate package from the one on melpa)

### Python-black
We use python-black for Emacs to automatically format python code on the go,
but on Windows this is slightly broken. We need the black and black-macchiato
python packages, but we need to manually modify black-macchiato so we won't get
any 'permission denied' errors [Errno 13]. Install black-macchiato as normally
from pypi, find the macchiato.py file in your python install and replace the
`format_lines` function with the following:

``` python
def format_lines(lines: List[str], black_args=None) -> List[str]:
    if black_args is None:
        black_args = []

    with tempfile.NamedTemporaryFile(
        suffix=".py", dir=os.getcwd(), mode="wt+", delete=False
    ) as fp:
        # Copy the input.
        fp.writelines(lines)
        fp.flush()

    # Run black.
    if "--quiet" not in black_args:
        black_args.append("--quiet")
    black_args.append(fp.name)

    try:
        exit_code = black.main(args=black_args)
    except SystemExit as exc:
        exit_code = exc.code

    if exit_code == 0:
        # Write output.
        lines = []
        with open(fp.name, mode="rt") as f:
            # fp.seek(0)
            lines = f.readlines()

        os.remove(fp.name)
        return cast(List[str], lines)

    os.remove(fp.name)

    raise RuntimeError("black failed", exit_code)
```

(I've added a file in lisp/black-macchiato.py that includes this, but it would
be best to not have a separate package from the one on pypi)

### Using the Projectil package (also form helm)
By default linux commands won't work on Windows, so e.g. `projectile-grep` (C-c
p s g) won't work and will likely give the error: `FIND: Parameter format not
correct`.We'll ammend this by installing *Cygwin* (a linux command libary for
Windows) and adding its execution directory (`/bin`) to our **PATH** in the
windows system environment variable.

Get Cygwin at https://www.cygwin.com/ or use chocolatey to install it:
``` powershell
choco get cygwin
```

Add the `/bin` directory of the *Cygwin* install directory to your **PATH**
system environment variable.

Finally we specifically need to tell Emacs which `find` command to use. Look in
the `personal.el` file and edit the follow line you see there to the `find.exe`
in `/bin` of your *Cygwin* install directory.

``` powershell
;; Set executable for the find command executable
(setq find-program "")
```

For example if you installed cygwin in `C:/` it'll be something like:
``` powershell
;; Set executable for the find command executable
(setq find-program "C:/cygwin64/bin/find.exe")
```

The links that helped resolve this issue:
https://github.com/bbatsov/projectile/issues/827
https://stackoverflow.com/questions/3918341/find-parameter-format-not-correct

### Running a Shell
Running a terminal/shell in Emacs on Windows is really, really awkward. There
are too many small (sometimes big) things that are broken and there aren't
fixes for all of it. For most use cases this is fine, but doing more advanced
stuff may result in errors. So it's honestly recommended to just use a terminal
of your choice next to Emacs. However, this config comes with a working
powershell, which of course can be used by calling `M-x powershell`.

Using ssh to interact with git may result in an error asking for a ssh_askpass,
this can be fixed by adding an environmental variable, in Windows, called
`SSH_ASKPASS` and setting it to where git-askpass.exe is installed (this comes
with the Windows installation of git), mine was at `C:\Program
Files\Git\mingw64\bin`

## Running update on annoying Windows things
1. Within Emacs (emacs-lisp) you may not be able to do certain function/program
   calls if it requires a windows path that includes a space, a common one is
   the `/Program Files/` directory.
