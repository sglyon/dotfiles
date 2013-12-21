function addprocs_odyny(n::Int)
    """
    Add processes on the desktop in my parents nyc apartment

    """
    addprocs(repmat(["sglyon@odyny.homeip.net:22"], n),
             tunnel=true,
             dir='/usr/local/julia/usr/bin')
end


function addprocs_home(n::Int)
    """
    Add processes on my home desktop

    """
    addprocs(repmat(["sglyon@sglyon.selfip.org:22"], n),
             tunnel=true,
             dir='/usr/local/julia/usr/bin')
end


macro run(x::Symbol)
    """
    Reloads the script ``x.jl``. The file name should be given without
    the ``.jl`` extension. So, for example, if I wanted to run
    ``foo.jl`` I would call ``@run foo``

    Notes
    -----
    Should feel like the IPython %run magic

    """
    reload(string(x) * ".jl")
end
