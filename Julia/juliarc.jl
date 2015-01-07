using Compat

function addprocs_odyny(n::Int)
    addprocs(repmat(["sglyon@odyny.homeip.net:22"], n),
             tunnel=true,
             dir="/usr/local/julia/usr/bin")
end


function addprocs_home(n::Int)
    addprocs(repmat(["sglyon@sglyon.selfip.org:22"], n),
             tunnel=true,
             dir="/usr/local/julia/usr/bin")
end

function addprocs_stern(n::Int)
    addprocs(repmat(["sgl290@128.122.185.159"], n),
             tunnel=true,
             dir="/usr/bin")
end

function addprocs_chase_stern(n::Int)
    addprocs(repmat(["chase@128.122.185.122"], n),
             tunnel=true,
             dir="/home/chase/Programming/julia/usr/bin")
end

function addprocs_dadpro(n::Int)
    addprocs(repmat(["sglyon@72.208.102.111"], n),
             tunnel=true,
             dir="/usr/local/julia/usr/bin")
end

function addprocs_gcloud(n::Int, ip::String)
    addprocs(repmat(["sglyon@$(ip)"], n),
             tunnel=true,
             dir="/home/sglyon/julia-release/usr/bin")
end


# gcloud constants
const gcloud_image_url = "https://www.googleapis.com/compute/v1/projects/sgl-julia/global/images/julia-src-deb-10212014"
const gcloud_options = Compat.@Dict("project" => "sgl-julia",
                                    "image" => gcloud_image_url)
const gcloud_unary_options = ["--no-boot-disk-auto-delete"]


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


macro timen(ex, n)
    quote
        local t0 = time_ns()
        for i = 1:$(esc(n))
            local val = $(esc(ex))
        end
        local t1 = time_ns()
        (t1 - t0) / 1.e9
    end
end

macro unpack(obj, fields...)
    Expr(:block, [Expr(:(=), esc(:($f)), :($obj.$f)) for f in fields]...)
end


# macro unpackall(obj)
#     Expr(:block, Expr(:(=), :nms, :(names($obj)))
# end

function save_prof(f_name="profile_resuts.txt")
    s = open(f_name,"w")
    Profile.print(s,cols = 500)
    close(s)
end


macro timeit(ex)
    quote
        local val = $(esc(ex))  # Warm up
        t = zeros(3)

        # Determine number of loops so that total time > 0.1s.
        n = 1
        for i = 0:9
            n = 10^i
            t[1] = @timen $(esc(ex)) n
            if t[1] > 0.1
                break
            end
        end

        # Two more production runs.
        for i = 2:3
            t[i] = @timen $(esc(ex)) n
        end
        best = minimum(t) / n

        # Format to nano-, micro- or milliseconds.
        if best < 1e-6
            best *= 1e9
            pre = "n"
        elseif best < 1e-3
            best *= 1e6
            pre = "\u00b5"
        else
            best *= 1e3
            pre = "m"
        end
        @printf "%d loops, best of 3: %4.2f %ss per loop\n" n best pre
    end
end


macro paste()
    include_string(clipboard());
end


## ------------------------- ##
#- Matrix to Latex functions -#
## ------------------------- ##

function pprint(x::Vector; fmt="%.4f", sep::String="  ")
    # the dofmt function taken from here:
    # https://groups.google.com/d/msg/julia-users/7Sn5yys0UJE/z33lMBqY5FsJ
    @eval dofmt(x) = @sprintf($fmt, x)
    join([dofmt(i) for i in x], sep)
end

function pprint(x::Matrix; fmt="%.4f", sep::String="  ")
    n = size(x, 1)
    join([pprint(squeeze(x[i, :], 1), fmt=fmt, sep=sep) for i=1:n], "\n")
end

tex_mat_guts(x::Vector; fmt::String="%.4f") = pprint(x; fmt=fmt, sep=" & ")

function tex_mat_guts(x::Matrix; fmt::String="%.4f")
    n = size(x, 1)
    join([tex_mat_guts(squeeze(x[i, :], 1), fmt=fmt) for i=1:n], " \\\\\n")
end


#mat_type should be one of p, b, v, V, B, small
function tex_str(x::Array; fmt::String="%.4f", mat_type="b")
    """\\begin{$(mat_type)matrix}
    $(tex_mat_guts(x, fmt=fmt))
    \\end{$(mat_type)matrix}
    """
end

## ------------------------------------------------------ ##
#- Print array to Numpy format for copy/paste into python -#
## ------------------------------------------------------ ##

numpy_guts(x::Vector) = "[" * join(x, ", ") * "]"
numpy_str(x::Vector) = "np.array(" * numpy_guts(x) * ")"


function numpy_str(x::Matrix)
    nr = size(x, 1)
    nc = size(x, 2)
    out = "np.array(["
    for row=1:nr
        out *= numpy_guts(x[row, :][:]) * ",\n"
    end
    out *= "])"
end


# function writemime(io::IO, ::MIME"text/latex", x::Vector; fmt::String="%.4f")
