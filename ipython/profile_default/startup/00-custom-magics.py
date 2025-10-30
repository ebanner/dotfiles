from IPython.core.magic import register_line_magic
from IPython.display import Image, display

@register_line_magic
def image(line):
    display(Image(line))
