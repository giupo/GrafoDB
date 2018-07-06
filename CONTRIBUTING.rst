============
Contributing
============

Contributions are welcome, and they are greatly appreciated! Every
little bit helps, and credit will always be given. 

You can contribute in many ways:

Types of Contributions
----------------------

Report Bugs
~~~~~~~~~~~

Report bugs at https://osiride-gitlab-dev.utenze.bankit.it/m024000/GrafoDB/issues.

If you are reporting a bug, please include:

* Your operating system name and version.
* Any details about your local setup that might be helpful in troubleshooting.
* Detailed steps to reproduce the bug (CRUCIAL!)

Fix Bugs
~~~~~~~~

Look through the GitLab issues for bugs 
(https://osiride-gitlab-dev.utenze.bankit.it/m024000/GrafoDB/issues). 
Anything tagged with "bug" is open to whoever wants to implement it.

Implement Features
~~~~~~~~~~~~~~~~~~

Any contribution is welcome. submit your code with tests and it will make it to 
a release.

Write Documentation
~~~~~~~~~~~~~~~~~~~

`GrafoDB` pacakge could always use more documentation.


Submit Feedback
~~~~~~~~~~~~~~~

The best way to send feedback is to file an issue at https://osiride-gitlab-dev.utenze.bankit.it/m024000/GrafoDB/issues

If you are proposing a feature:

* Explain in detail how it would work.
* Keep the scope as narrow as possible, to make it easier to implement.
* Remember that this is a sidekick-driven project, and that contributions
  are welcome :)

Get Started!
------------

Ready to contribute? Here's how to set up `GrafoDB` for local development.

1. Fork the `GrafoDB` repo on GitLab.
2. Clone your fork locally::

    $ git clone https://osiride-gitlab-dev.utenze.bankit.it/<your_user_id_here>/GrafoDB.git

3. Make sure you have all the deps installed (your local copy into a virtualenv. Assuming you have `devtools` installed, this is how you set up your fork for local development::

    $ cd GrafoDB && R
    > require(devtools)
    > load_all() 
    

4. Create a branch for local development::

    $ git checkout -b name-of-your-bugfix-or-feature
   
   Now you can make your changes locally.

5. Test your feature out of the box (write tests!!!). It's advisable to set `GRAFODB_ENV` to `test`:

    > Sys.setenv(GRAFODB_ENV='test')
    > devtools::test()
   
6. Commit your changes and push your branch to GitHub::

    $ git add .
    $ git commit -m "Your detailed description of your changes."
    $ git push origin name-of-your-bugfix-or-feature

7. Submit a pull request through the GitLab website (https://osiride-gitlab-dev.utenze.bankit.it/)

