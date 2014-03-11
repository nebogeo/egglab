# script for generating base egg pattern images

import random
import math
from PIL import Image, ImageDraw, ImageOps

def noise(x, y, per, perm, dirs):
    def surflet(gridX, gridY):
        distX, distY = abs(x-gridX), abs(y-gridY)
        polyX = 1 - 6*distX**5 + 15*distX**4 - 10*distX**3
        polyY = 1 - 6*distY**5 + 15*distY**4 - 10*distY**3
        hashed = perm[perm[int(gridX)%per] + int(gridY)%per]
        grad = (x-gridX)*dirs[hashed][0] + (y-gridY)*dirs[hashed][1]
        return polyX * polyY * grad
    intX, intY = int(x), int(y)
    return (surflet(intX+0, intY+0) + surflet(intX+1, intY+0) +
            surflet(intX+0, intY+1) + surflet(intX+1, intY+1))

def fBm(x, y, per, octs, perm, dirs):
    val = 0
    for o in range(octs):
        val += 0.5**o * noise(x*2**o, y*2**o, per*2**o, perm, dirs)
    val=min(val,0.5)
    val=max(val,-0.5)
    return int((0.5+val)*255)

def make_noise(size,freq,inv):
    perm = range(256)
    random.shuffle(perm)
    perm += perm
    dirs = [(math.cos(a * 2.0 * math.pi / 256),
             math.sin(a * 2.0 * math.pi / 256))
            for a in range(256)]
    octs, data = 5, []
    for y in range(size):
        for x in range(size):
            val = fBm(x*freq, y*freq, int(size*freq), octs, perm, dirs)
            data.append((val,val,val,val))
    im = Image.new("RGBA", (size, size))
    im.putdata(data)
    return im

def invert(image):
    def invert(image):
        return image.point(lambda p: 255 - p)
    r, g, b, a = image.split()
    r, g, b = map(invert, (r, g, b))
    return Image.merge(image.mode, (r, g, b, a))

def threshold(image,threshold):
    return image.point(lambda p: p > threshold and 255)

def gen_noise(scale,thresh,var,inv):
    inv_v=256
    im = make_noise(128, 1/float(scale), inv_v)
    if thresh:
        im = threshold(im,thresh)
        if inv=="black": im = invert(im)
        im.save("images/v"+str(var)+"/"+inv+"-s"+str(scale)+"-t"+str(thresh)+".png")
    else:
        if inv=="black": im = invert(im)
        im.save("images/v"+str(var)+"/"+inv+"-s"+str(scale)+".png")

scales = [8,16,32,64]
thresholds = [94,160]
num_vars = 8

for v in range(0,num_vars):
    for s in scales:
        gen_noise(s,False,v,"black")
    for s in scales:
        for t in thresholds:
            gen_noise(s,t,v,"black")

for v in range(0,num_vars):
    for s in scales:
        gen_noise(s,False,v,"white")
    for s in scales:
        for t in thresholds:
            gen_noise(s,t,v,"white")

#image = threshold(image,127)
#image.save("PerlinNoise.png", "PNG")
