load(
    "@prelude//cfg/modifier:cfg_constructor.bzl",
    _cfg_constructor_post_constraint_analysis = "cfg_constructor_post_constraint_analysis",
    _cfg_constructor_pre_constraint_analysis = "cfg_constructor_pre_constraint_analysis",
)

def stage0(**kwargs):
    if kwargs.get("aliases") == None:
        kwargs["aliases"] = struct(
            release = "dev_buck//mode:release",
            debug = "dev_buck//mode:debug",
        )
    if kwargs.get("extra_data") == None:
        kwargs["extra_data"] = struct()
    return _cfg_constructor_pre_constraint_analysis(**kwargs)

def stage1(**kwargs):
    return _cfg_constructor_post_constraint_analysis(**kwargs)
