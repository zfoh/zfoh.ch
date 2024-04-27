"use strict";

function lerp(x, y, t) {
    const out = new Array(x.length);
    for(let i = 0; i < x.length; i++) {
        out[i] = x[i] * (1 - t) + y[i] * t;
    }
    return out;
}

function gradient(c0, c1) {
    return function(t) {
        return lerp(c0, c1, t);
    };
}

function zurihac2024logo(scheme) {
    if (!scheme) {
        scheme = {
            "front": gradient(
                [211/255, 32/255, 32/255, 1],
                [234/255, 137/255, 109/255, 1],
            ),
            "side": gradient(
                [211/255, 32/255, 32/255, 1],
                [234/255, 137/255, 109/255, 1],
            ),
            "top": gradient(
                [230/255, 196/255, 146/255, 1],
                [218/255, 163/255, 81/255, 1],
            ),
            "shade": gradient(
                [0.3, 0.3, 0.4, 1],
                [0.2, 0.2, 0.3, 1],
            ),
        }
    }

    function color(cf, f) {
        return f(function(p) {return [p, cf(p)]});
    }

    function empty() {
        return {
            "points": [],
            "indices": [],
        };
    }

    function triangle(p0, p1, p2) {
        return {
            "points": [p0, p1, p2],
            "indices": [0, 1, 2],
        };
    }

    function rectangle(p0, p1, p2, p3) {
        return {
            "points": [p0, p1, p2, p3],
            "indices": [0, 1, 2, 0, 2, 3],
        };
    }

    function triangles(m) {
        let out = [];
        for(let i = 0; i < m.indices.length; i += 3) {
            out = out.concat([[
                m.points[m.indices[i + 0]][0],
                m.points[m.indices[i + 1]][0],
                m.points[m.indices[i + 2]][0]
            ]]);
        }
        return out;
    }

    function edges(m) {
        return [].concat(...triangles(m).map(function(triangle) {
            const [p0, p1, p2] = triangle;
            return [[p0, p1], [p1, p2], [p2, p0]];
        }));
    }

    function glue(l) {
        let points = [];
        let indices = [];
        let offset = 0;
        for(const m of l) {
            points = points.concat(m.points);
            for(const idx of m.indices) {
                indices = indices.concat(offset + idx);
            }
            offset += m.points.length;
        }
        return {
            "points": points,
            "indices": indices,
        };
    }

    function font(h) {
        const lw = h * 0.15;
        function rectangles(p, rects) {
            return glue(rects.map(function (r) {
                return rectangle(p(r[0]), p(r[1]), p(r[2]), p(r[3]));
            }));
        }
        return {
            "z": {
                "width": 1,
                "draw": function(w, point) {
                    const zw = lw * 1.2;
                    return rectangles(point, [
                        [[0, 0], [w, 0], [w, lw], [0, lw]],
                        [[0, h], [w, h], [w, h - lw], [0, h - lw]],
                        [[w - zw, h - lw], [w, h - lw], [zw, lw], [0, lw]]
                    ]);
                }
            },
            "u": {
                "width": 1,
                "draw": function(w, point) {
                    return rectangles(point, [
                        [[0, 0], [lw, 0], [lw, h], [0, h]],
                        [[0, 0], [w, 0], [w, lw], [0, lw]],
                        [[w - lw, 0], [w, 0], [w, h], [w - lw, h]]
                    ]);
                }
            },
            "r": {
                "width": 1,
                "draw": function(w, point) {
                    const himid = (h + lw) * 0.5;
                    const lomid = (h - lw) * 0.5;
                    return rectangles(point, [
                        [[0, 0], [lw, 0], [lw, h], [0, h]],  // left
                        [[0, h], [w, h], [w, h - lw], [0, h - lw]],  // top
                        [[w, h], [w, lomid], [w - lw, lomid], [w - lw, h]],  // right
                        [[0, himid], [w, himid], [w, lomid], [0, lomid]],  // middle
                        [[lw, lomid], [lw + lw, lomid], [w, 0], [w - lw, 0]]  // slant
                    ]);
                }
            },
            "i": {
                "width": 0.3,
                "draw": function(w, point) {
                    const s = (w - lw) * 0.5;
                    return rectangles(point, [
                        [[s, 0], [s + lw, 0], [s + lw, h], [s, h]]
                    ]);
                }
            },
            "h": {
                "width": 1,
                "draw": function(w, point) {
                    const himid = (h + lw) * 0.5;
                    const lomid = (h - lw) * 0.5;
                    return rectangles(point, [
                        [[0, 0], [lw, 0], [lw, h], [0, h]],  // left
                        [[0, himid], [w, himid], [w, lomid], [0, lomid]],  // mid
                        [[w - lw, 0], [w, 0], [w, h], [w - lw, h]]  // right
                    ]);
                }
            },
            "a": {
                "width": 1,
                "draw": function(w, point) {
                    const himid = (h + lw) * 0.5;
                    const lomid = (h - lw) * 0.5;
                    return rectangles(point, [
                        [[0, 0], [lw, 0], [lw, h], [0, h]],  // left
                        [[0, himid], [w, himid], [w, lomid], [0, lomid]],  // mid
                        [[w - lw, 0], [w, 0], [w, h], [w - lw, h]],  // right
                        [[0, h], [w, h], [w, h - lw], [0, h - lw]]  // top
                    ]);
                }
            },
            "c": {
                "width": 1,
                "draw": function(w, point) {
                    const himid = (h + lw) * 0.5;
                    const lomid = (h - lw) * 0.5;
                    return rectangles(point, [
                        [[0, 0], [lw, 0], [lw, h], [0, h]],  // left
                        [[0, h], [w, h], [w, h - lw], [0, h - lw]],  // top
                        [[0, 0], [0, lw], [w, lw], [w, 0]]  // bottom
                    ]);
                }
            },
            "'": {
                "width": 0.3,
                "draw": function(w, point) {
                    const himid = (h + lw) * 0.5;
                    const s = (w - lw) * 0.5;
                    return rectangles(point, [
                        [[s, h], [s + lw, h], [s + lw, himid], [s, himid]]
                    ]);
                }
            },
            "2": {
                "width": 1,
                "draw": function(w, point) {
                    const himid = (h + lw) * 0.5;
                    const lomid = (h - lw) * 0.5;
                    return rectangles(point, [
                        [[0, lomid], [lw, lomid], [lw, 0], [0, 0]],  // left
                        [[0, himid], [w, himid], [w, lomid], [0, lomid]],  // mid
                        [[w - lw, himid], [w, himid], [w, h], [w - lw, h]],  // right
                        [[0, h], [w, h], [w, h - lw], [0, h - lw]],  // top
                        [[0, 0], [0, lw], [w, lw], [w, 0]]  // bottom
                    ]);
                }
            },
            "4": {
                "width": 1,
                "draw": function(w, point) {
                    const himid = (h + lw) * 0.5;
                    const lomid = (h - lw) * 0.5;
                    return rectangles(point, [
                        [[0, h], [lw, h], [lw, himid], [0, himid]],  // left
                        [[0, himid], [w, himid], [w, lomid], [0, lomid]],  // mid
                        [[w - lw, 0], [w, 0], [w, h], [w - lw, h]]  // right
                    ]);
                }
            }
        };
    }

    function text(glyphs, str, w, point) {
        const space = 0.3;
        let totalCharWidth = 0;
        for(const c of str) {
            totalCharWidth += glyphs[c].width;
        }
        const totalWidth = totalCharWidth + (str.length - 1) * space;
        let out = [];
        let x = 0;
        for(let i = 0; i < str.length; i++) {
            const cw = glyphs[str[i]].width / totalWidth * w;
            out = out.concat(
                glyphs[str[i]].draw(cw, function(xy) {
                    return point([xy[0] + x, xy[1]]);
                })
            );
            x += space / totalWidth * w + cw;
        };
        return glue(out);
    }

    function tw(lw, point) {
        const space = lw * 0.5;
        const slope = 0.4;

        // > shape
        const bx = -slope * lw;
        const b = glue([
            // Lower
            rectangle(
                point([0, 0]),
                point([0, -bx / slope]),
                point([bx + 0.5 * slope, 0.5]),
                point([bx + lw, 0]),
            ),
            triangle(
                point([bx + lw, 0]),
                point([bx + 0.5 * slope + lw, 0.5]),
                point([bx + 0.5 * slope, 0.5])
            ),
            // Upper
            rectangle(
                point([0, 1]),
                point([bx + lw, 1]),
                point([bx + 0.5 * slope, 0.5]),
                point([0, 1 + bx / slope])
            ),
            triangle(
                point([bx + lw, 1]),
                point([bx + 0.5 * slope + lw, 0.5]),
                point([bx + 0.5 * slope, 0.5])
            )
        ]);

        // lambda shape
        const lx = bx + lw + space;
        const l = glue([
            rectangle(
                point([lx + 0, 0]),
                point([lx + lw, 0]),
                point([lx + 0.5 * slope + lw, 0.5]),
                point([lx + 0.5 * slope, 0.5])
            ),
            rectangle(
                point([lx + 0, 1]),
                point([lx + lw, 1]),
                point([lx + slope + lw, 0]),
                point([lx + slope, 0])
            )
        ]);

        // = shape
        const e1y = 0.5 + space * 0.5 + lw;
        const e2y = 0.5 - space * 0.5;
        const e1x = lx + lw + space + slope * (1 - e1y);
        const e2x = lx + lw + space + slope * (1 - e2y);
        const e = glue([
           rectangle(
               point([e1x, e1y]),
               point([1, e1y]),
               point([1, e1y - lw]),
               point([e1x + slope * lw, e1y - lw])
           ),
           rectangle(
               point([e2x, e2y]),
               point([1, e2y]),
               point([1, e2y - lw]),
               point([e2x + slope * lw, e2y - lw])
           ),
        ]);

        return glue([b, l, e]);
    };

    function sidez(lw, point) {
        const zw = lw * 1.5;
        return glue([
            rectangle(
                point([0, 0]),
                point([1, 0]),
                point([1, lw]),
                point([0, lw])
            ),
            rectangle(
                point([0, 1]),
                point([1, 1]),
                point([1, 1 - lw]),
                point([0, 1 - lw])
            ),
            rectangle(
                point([1 - zw, 1 - lw]),
                point([1, 1 - lw]),
                point([zw, lw]),
                point([0, lw])
            )
        ]);
    };

    function frontcol(p) {
        return scheme.front(p[1]);
    };
    function sidecol(p) {
        return scheme.side(p[1]);
    };
    function topcol(p) {
        return scheme.top(p[0]);
    };
    function shadecol (p) {
        return scheme.shade(p[1]);
    };
    const cubelw = 0.2;
    const front = tw(cubelw, function(xy) {
        return color(sidecol, function(point) {
            return point([xy[0], xy[1], 1.0]);
        });
    });
    const back = tw(cubelw, function(xy) {
        return color(sidecol, function(point) {
            return point([xy[0], xy[1], 0]);
        });
    });
    const fonth = 0.18;
    const glyphs = font(fonth);
    return glue([
        front,
        glue(edges(front).map(function(edge) {
            const fp = function(p) {return [p[0], p[1], p[2] - 1.0];};
            const [p0, q0] = edge;
            const p1 = fp(p0);
            const q1 = fp(q0);
            const rec = function(p) {
                return rectangle(p(p0), p(q0), p(q1), p(p1));
            };
            if(p0[1] == q0[1]) {
                return color(topcol, rec);
            } else if(p0[0] == 0 && q0[0] == 0) {
                return empty();  // Prevent colliding with z.
            } else if(p0[0] == 1 && q0[0] == 1) {
                return color(frontcol, rec);
            } else {
                return color(shadecol, rec);
            }
        })),
        back,
        sidez(cubelw, function(xy) {
            return color(frontcol, function(point) {
                return point([0.0, xy[1], xy[0]]);
            });
        }),
        color(
            frontcol,
            function(point) {
                return text(glyphs, "zurihac'24", 1, function(p) {
                    return point([1, 1 - fonth + p[1], p[0]]);
                });
            }
        )
    ]);
}
