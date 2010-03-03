Golbarg is a static blog generator written in Python. It is heavily inspired by
Jekyll_, but is is (obviously!) better (at least in my opinion).

You can see it in action on `my own blog`_, (source code on Github_ too).

Golbarg is free software, available under the terms of the GNU GPLv3_ license.

.. _Jekyll: http://wiki.github.com/mojombo/jekyll/
.. _my own blog: http://schnouki.net/
.. _Github: http://github.com/Schnouki/schnouki.net
.. _GPLv3: http://www.fsf.org/licensing/licenses/gpl.html

Features
========

* Markdown_ synax for editing posts
* `Jinja 2`_ template engine
* YAML_ standard for configuration and posts metadata (similar to Jekyll's
  `YAML front matter`_)
* Written in Python_
* Well-documented and easily hackable
* Built-in support for tags and archive generation

.. _Markdown: http://daringfireball.net/projects/markdown/
.. _Jinja 2: http://jinja.pocoo.org/2/
.. _YAML: http://www.yaml.org/
.. _YAML front matter: http://wiki.github.com/mojombo/jekyll/yaml-front-matter
.. _Python: http://www.python.org/

Dependencies
============

* `python-markdown <http://www.freewisdom.org/projects/python-markdown/>`_
* `Jinja 2 <http://jinja.pocoo.org/2/>`_
* `PyYAML <http://pyyaml.org/>`_


Installation
============

1. Install Python (version 2.6, Golbarg is *not* compatible with Python 3 yet)
#. Install the dependencies
#. Install Golbarg: you can use Python tools (``easy_install Golbarg`` or ``pip
   install Golbarg``) or, if you are a Linux user, the tools from your
   distribution(``pacman``, ``aptitude``, ``emerge``...) if someone packaged
   Golbarg for it
#. Create a directory where you will put all your files
#. Create your configuration file and templates; you may want to look at 
   `mine <http://github.com/Schnouki/schnouki.net>`_ for examples
#. Write your posts in the content/posts directory, using the Markdown syntax,
   with a YAML header for metadata (again, see my blog for examples)
#. Run ``golbarg`` to generate the full site
#. Upload it somewhere and enjoy your blog!


``golbarg-mode`` for Emacs
==========================

If you use Emacs, you may want to use ``golbarg-mode``. Simply add the directory
where ``golbarg.el`` is located to your ``load-path``, add ``(require
'golbarg)`` in your ``.emacs``, and you are done. You can now use ``M-x
golbarg-mode`` to switch a buffer to the Golbarg major mode, and use other
commands such as ``golbarg-new-draft``, ``golbarg-publish-post``, and
``golbarg-preview`` (after customizing the ``golbarg-drafts-dir`` and
``golbarg-posts-dir`` variables).

You can see a full configuration on my `emacs-config
<http://github.com/Schnouki/emacs-config/blob/9aee67d153f63669af99626a14ac39e94eddeff7/init-30-modes.el#L60>`_
repository.

``golbarg-mode`` requires ``markdown-mode`` and ``yaml-mode`` to work, so be
sure to install these modes first.
