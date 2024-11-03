/**
 * NaNsense (JavaScript code generator)
 * Author - Kithinji Brian
 * Version - 0.0.1
 */ 

class Vec3D {
    constructor({ x, y, z }) {
        this.x = x;
        this.y = y;
        this.z = z;
    }
}

let r = new Vec3D({ x: 1, y: 2, z: 3 }).__add__(new Vec3D({ x: 1, y: 2, z: 3 }));

