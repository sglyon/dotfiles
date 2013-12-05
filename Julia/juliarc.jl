function addprocs_odyny(n::Int)
    addprocs(repmat(["sglyon@odyny.homeip.net:22"], n),
             tunnel=true,
             dir="/Users/sglyon/.src/julia/usr/bin")
end


function addprocs_home(n::Int)
    addprocs(repmat(["sglyon@sglyon.selfip.org:22"], n),
             tunnel=true,
             dir="/Users/spencerlyon2/.src/julia/usr/bin")
end
