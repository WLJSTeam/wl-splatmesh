let SplatMesh = undefined;

core.SplatMesh = async (args, env) => {
    const type = await interpretate(args[1], env);
    if (type != "Packed") throw 'SplatMesh must be packed';

    const payload = await interpretate(args[2], env);
    if (!(payload instanceof ArrayBuffer)) throw 'SplatMesh does not have ByteArray';

    if (!SplatMesh) SplatMesh = (await import('./spark.js')).SplatMesh;
    const p = new Deferred();
    const splat = new SplatMesh({ fileBytes:  payload, onLoad: () => p.resolve()});
    await p.promise;

    const THREE = interpretate.shared.THREE.THREE;

    const box = splat.getBoundingBox();
    const helper = new THREE.Box3Helper( box, 0xffff00 );
    helper.material.opacity = 0;
    helper.material.visible = false;
    //throw splat;
    

    env.local.splat = splat;
    env.local.helper = helper;

    env.mesh.add(helper);
    env.mesh.add(splat);
    return splat;
}

core.SplatMesh.destroy = (args, env) => {
    env.local.splat.dispose();
    env.local.helper.dispose();
}

core.SplatMesh.update = async (args, env) => {
    throw 'Updates of SplatMesh are not supported';
}

core.SplatMesh.virtual = true;

core["CoffeeLiqueur`Workshop`SplatMesh`"] = core.SplatMesh;