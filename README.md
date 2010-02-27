Golbarg is a static blog generator written in Python. It is heavily inspired by [Jekyll](http://wiki.github.com/mojombo/jekyll/), but is is (obviously!) better (at least in my opinion).

You can see it in action on [my own blog](http://schnouki.net/) (source code [on Github](http://github.com/Schnouki/schnouki.net) too).

Golbarg is free software, available under the terms of the [GNU GPLv3](http://www.fsf.org/licensing/licenses/gpl.html) license.


Features
========

* [Markdown](http://daringfireball.net/projects/markdown/) synax for editing posts
* [Jinja 2](http://jinja.pocoo.org/2/) template engine
* [YAML](http://www.yaml.org/) standard for configuration and posts metadata (similar to Jekyll's [YAML front matter](http://wiki.github.com/mojombo/jekyll/yaml-front-matter))
* Written in [Python](http://www.python.org/)
* Well-documented and easily hackable
* Built-in support for tags and archive generation


Dependencies
============

* [python-markdown](http://www.freewisdom.org/projects/python-markdown/)
* [Jinja 2](http://jinja.pocoo.org/2/)
* [PyYAML](http://pyyaml.org/)


Installation
============

1. Install Python (version 2.6, Golbarg is *not* compatible with Python 3 yet)
2. Install the dependencies
3. Install Golbarg:
    * Using Python tools: `easy_install Golbarg` or `pip Golbarg`
    * Using tools from your distribution (`pacman`, `aptitude`, `emerge`...) if someone packaged Golbarg for it
4. Create a directory where you will put all your files
5. Create your configuration file and templates; you may want to look at [mine](http://github.com/Schnouki/schnouki.net) for examples
6. Write your posts in the content/posts directory, using the Markdown syntax, with a YAML header for metadata (again, see my blog for examples)
7. Run `golbarg` to generate the full site
8. Upload it somewhere and enjoy your blog!
