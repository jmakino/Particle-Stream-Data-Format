import math

def distance_squared(v, w):
    """Computes the distance-squared between two vectors."""
    dx = 0.0
    for x,y in zip(v,w):
        d = x-y
        dx += d*d
    return dx

def distance(v,w):
    """Computes distance between two vectors."""
    return math.sqrt(distance_squared(v,w))

def dot(v,w):
    """Dot product."""
    return sum([x*y for x,y in zip(v,w)])

def norm(v):
    """Euclidean norm of a vector."""
    return math.sqrt(dot(v,v))
