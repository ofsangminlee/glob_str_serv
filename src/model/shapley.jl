## Shapley.jl 
# Shapley decomposition of mechanisms

## Load packages and necessary files
using DataFrames, JLD2, Combinatorics, LinearAlgebra, PrettyTables, Statistics, CSV

@load "./output/model/model_input.jld2" # Model parameters and primitives
for i in 1:ncol(model_input)
    obj = names(model_input)[i]
    eval(Meta.parse(string(obj, " = model_input.", obj, "[1]")))
end

@load "./output/model/models.jld2" # Models

## Functions for decomposition
# Function for a country-year
# Output: sectoral labor shares
# Inputs: input-output matrix, sectoral prices, sectoral consumption, and per-capita sectoral net exports
derive_lshare_inner = function (; ψ_cty, P_cty, C_cty, NXp_cty)
    B = ψ_cty
    P = P_cty
    C = C_cty
    NX = NXp_cty
    W_L_sec_div_L = ((I - transpose(B)) * ones(3)) .* (inv(I - B) * (P .* C .+ NX))
    return (W_L_sec_div_L ./ sum(W_L_sec_div_L))
end

# Sanity check
L11 = (m_base.L_sec[1][1, :]) ./ (sum(m_base.L_sec[1][1, :]))

L11_wl = derive_lshare_inner(
    ψ_cty=m_base.ψ[1][1, :, :],
    P_cty=m_base.P_sec[1][1, :],
    C_cty=m_base.C_sec[1][1, :],
    NXp_cty=m_base.NX_sec[1][1, :] ./ sum(m_base.L_sec[1][1, :])
)

@assert isapprox(L11, L11_wl; rtol=1e-3)

# Output: sectoral labor shares
# Inputs: input-output matrix, sectoral prices, real income, and per-capita sectoral net exports
# Compared to the previous one, this is the outer function.
# This is to calculate C_cty (cectoral consumptions)from P_cty (sectoral prices) and Cf_cty (real income)
derive_lshare = function (; ψ_cty, P_cty, Cf_cty, NXp_cty, ϕ_cty, σ, ϵ)
    P_f = sum(ϕ_cty .* (P_cty .^ (1 - σ)) .* Cf_cty .^ ((1 - σ) .* (ϵ .- 1)))^(1 / (1 - σ))
    C_cty = ϕ_cty .* ((P_cty ./ P_f) .^ (-σ)) .* (Cf_cty .^ ((1 - σ) .* (ϵ .- 1) .+ 1))
    res = derive_lshare_inner(ψ_cty=ψ_cty, P_cty=P_cty, C_cty=C_cty, NXp_cty=NXp_cty)
    return (res)
end

L11_wl_2 = derive_lshare(;
    ψ_cty=m_base.ψ[1][1, :, :],
    P_cty=m_base.P_sec[1][1, :],
    Cf_cty=m_base.C_f[1][1],
    NXp_cty=m_base.NX_sec[1][1, :] ./ sum(m_base.L_sec[1][1, :]),
    ϕ_cty=ϕ[1, :],
    σ,
    ϵ
)

@assert isapprox(L11, L11_wl_2; rtol=1e-3)

## Functions for Shapley Decomposition
# Inner function
# Output: contributions
# Inputs: initial and final values of all variables
function shapley_decomposition(derive_lshare;
    ψ_cty_init, P_cty_init, Cf_cty_init, NXp_cty_init,
    ψ_cty_fin, P_cty_fin, Cf_cty_fin, NXp_cty_fin,
    ϕ_cty, σ, ϵ)

    variables = [:ψ_cty, :P_cty, :Cf_cty, :NXp_cty]
    perms = collect(permutations(variables))

    # Store total contributions (as vectors) per variable
    contribs = Dict(var => zeros(size(derive_lshare(;
        ψ_cty=ψ_cty_init, P_cty=P_cty_init, Cf_cty=Cf_cty_init, NXp_cty=NXp_cty_init,
        ϕ_cty=ϕ_cty, σ=σ, ϵ=ϵ))) for var in variables)

    for perm in perms
        # Start with initial values
        state = Dict(
            :ψ_cty => ψ_cty_init,
            :P_cty => P_cty_init,
            :Cf_cty => Cf_cty_init,
            :NXp_cty => NXp_cty_init
        )

        finals = Dict(
            :ψ_cty => ψ_cty_fin,
            :P_cty => P_cty_fin,
            :Cf_cty => Cf_cty_fin,
            :NXp_cty => NXp_cty_fin
        )

        L_prev = derive_lshare(; state..., ϕ_cty=ϕ_cty, σ=σ, ϵ=ϵ)

        for var in perm
            # Update variable to final
            state[var] = finals[var]
            L_next = derive_lshare(; state..., ϕ_cty=ϕ_cty, σ=σ, ϵ=ϵ)

            # Marginal contribution of this variable in this step
            contribs[var] .+= (L_next .- L_prev)

            # Prepare for next step
            L_prev = copy(L_next)
        end
    end

    # Average across all 24 paths
    for var in variables
        contribs[var] ./= length(perms)
    end

    return contribs
end

# Outer function
# Output: contributions 
# Input: a model and an index for a country
function shapley_model_country(i_model, i_country)
    res_decomp = shapley_decomposition(derive_lshare;
        ψ_cty_init=i_model.ψ[1][i_country, :, :],
        P_cty_init=i_model.P_sec[1][i_country, :],
        Cf_cty_init=i_model.C_f[1][i_country],
        NXp_cty_init=i_model.NX_sec[1][i_country, :] ./ sum(i_model.L_sec[1][i_country, :]),
        ψ_cty_fin=i_model.ψ[24][i_country, :, :],
        P_cty_fin=i_model.P_sec[24][i_country, :],
        Cf_cty_fin=i_model.C_f[24][i_country],
        NXp_cty_fin=i_model.NX_sec[24][i_country, :] ./ sum(i_model.L_sec[24][i_country, :]),
        ϕ_cty=ϕ[i_country, :],
        σ,
        ϵ
    )
    res_decomp = DataFrame(res_decomp)
    res_decomp[!, :tot] = [sum(row) for row in eachrow(res_decomp)]
    res_decomp[!, :country] .= list_countries[i_country]
    res_decomp[!, :ind] = ["g", "bts", "hts"]
    rename!(res_decomp, :ψ_cty => "io", :P_cty => "price", :Cf_cty => "income", :NXp_cty => "nx")
    for col in [:io, :price, :income, :nx, :tot]
        res_decomp[!, col] .*= 100 # Make all columns into percentage.
    end
    ## Make the relative contributions (Note: it's percentage of percentage, so proceed with caution with interpretation.)
    for col in [:io, :price, :income, :nx]
        res_decomp[!, Symbol(col, "_rel")] = res_decomp[!, col] ./ res_decomp[!, :tot] .* 100
    end
    return res_decomp
end

# Sanity check
res_init = derive_lshare(;
    ψ_cty=m_base.ψ[1][1, :, :],
    P_cty=m_base.P_sec[1][1, :],
    Cf_cty=m_base.C_f[1][1],
    NXp_cty=m_base.NX_sec[1][1, :] ./ sum(m_base.L_sec[1][1, :]),
    ϕ_cty=ϕ[1, :],
    σ,
    ϵ
)

res_fin = derive_lshare(;
    ψ_cty=m_base.ψ[24][1, :, :],
    P_cty=m_base.P_sec[24][1, :],
    Cf_cty=m_base.C_f[24][1],
    NXp_cty=m_base.NX_sec[24][1, :] ./ sum(m_base.L_sec[24][1, :]),
    ϕ_cty=ϕ[1, :],
    σ,
    ϵ
)

check1 = (res_fin .- res_init) .* 100

# Sanity check for one country
check2 = (shapley_model_country(m_base, 1)).tot
@assert isapprox(check1, check2; rtol=1e-3)





## Decomposition exercises
res = vcat([shapley_model_country(m_base, i) for i in 1:N]...) # Baseline for all countries

# Takeaways: price and nx are the key drivers.
describe(res[abs.(res.tot).>1, :])
describe(res[abs.(res.tot).>5, :])
describe(res[abs.(res.tot).>10, :])

# Why is the income effect negative?
# All but one country: income increased.
sum(m_base.C_f[1] .< m_base.C_f[24])

# Negative income effect
# 79 observations have negative income effects.
# 19 are because the direction of change is the opposite.
# 60 are because of input-output linkages.
inc_neg_tot = sum(abs.(res.tot).>1 .&& (res.income .< 0))

inc_neg_reverse = nrow(res[abs.(res.tot).>1 .&& (res.income .< 0) .&& (res.ind .== "g") .&& (res.tot .> 0), ["country", "ind", "income", "tot"]]) + nrow(res[abs.(res.tot).>1 .&& (res.income .< 0) .&& (res.ind .== "hts") .&& (res.tot .< 0), ["country", "ind", "income", "tot"]]) +
nrow(res[abs.(res.tot).>1 .&& (res.income .< 0) .&& (res.ind .== "bts") .&& (res.tot .< 0), ["country", "ind", "income", "tot"]])

inc_neg_io = nrow(res[abs.(res.tot).>1 .&& (res.income .< 0) .&& (res.ind .== "g") .&& (res.tot .< 0), ["country", "ind", "income", "tot"]]) + 
nrow(res[abs.(res.tot).>1 .&& (res.income .< 0) .&& (res.ind .== "hts") .&& (res.tot .> 0), ["country", "ind", "income", "tot"]]) +
nrow(res[abs.(res.tot).>1 .&& (res.income .< 0) .&& (res.ind .== "bts") .&& (res.tot .> 0), ["country", "ind", "income", "tot"]])


# Write explanation to text file
open("./doc/numbers/negative_income_explanation.txt", "w") do io
    write(io, "There are $inc_neg_tot country-sectors exhibited the negative income effects. Among those $inc_neg_reverse observations were because the changes in the sectoral shares were not consistent with the general pattern of structural transformation, $inc_neg_io observations were due to the Leontief inverse.")
end




## Now to figure out whether A and τ, which are the main ones.
# More complicated version of Shapley Decomposition.
function shapley_model_country_comp(f_model, f_year, s_model, s_year, i_country)
    res_decomp = shapley_decomposition(derive_lshare;
        ψ_cty_init=f_model.ψ[f_year][i_country, :, :],
        P_cty_init=f_model.P_sec[f_year][i_country, :],
        Cf_cty_init=f_model.C_f[f_year][i_country],
        NXp_cty_init=f_model.NX_sec[f_year][i_country, :] ./ sum(f_model.L_sec[f_year][i_country, :]),
        ψ_cty_fin=s_model.ψ[s_year][i_country, :, :],
        P_cty_fin=s_model.P_sec[s_year][i_country, :],
        Cf_cty_fin=s_model.C_f[s_year][i_country],
        NXp_cty_fin=s_model.NX_sec[s_year][i_country, :] ./ sum(s_model.L_sec[s_year][i_country, :]),
        ϕ_cty=ϕ[i_country, :],
        σ,
        ϵ
    )
    res_decomp = DataFrame(res_decomp)
    res_decomp[!, :tot] = [sum(row) for row in eachrow(res_decomp)]
    res_decomp[!, :country] .= list_countries[i_country]
    res_decomp[!, :ind] = ["g", "bts", "hts"]
    rename!(res_decomp, :ψ_cty => "io", :P_cty => "price", :Cf_cty => "income", :NXp_cty => "nx")
    for col in [:io, :price, :income, :nx, :tot]
        res_decomp[!, col] .*= 100 # Make all columns into percentage.
    end
    return res_decomp
end

cols_to_rename = [:io, :price, :income, :nx, :tot]

function a_t_decomp(i_country)
    # First, change A and then τ
    init_to_ya_nt = shapley_model_country_comp(m_base, 1, m_tau, 24, i_country) # Initial equilibrium to Yes A & change No τ change equilibrium
    rename!(init_to_ya_nt, [col => Symbol(string(col) * "_a") for col in cols_to_rename])

    ya_nt_to_final = shapley_model_country_comp(m_tau, 24, m_base, 24, i_country) # ya_nt to final equilibrium
    rename!(ya_nt_to_final, [col => Symbol(string(col) * "_t") for col in cols_to_rename])

    res_country = innerjoin(init_to_ya_nt, ya_nt_to_final, on=[:country, :ind], makeunique=true)

    res_country.tot = res_country.tot_a .+ res_country.tot_t

    @assert isapprox(res_country.tot, shapley_model_country(m_base, i_country).tot; rtol=1e-3)

    # Second, change τ and then A.
    init_to_nt_ya = shapley_model_country_comp(m_base, 1, m_prd, 24, i_country)
    rename!(init_to_nt_ya, [col => Symbol(string(col) * "_t") for col in cols_to_rename])

    nt_ya_to_final = shapley_model_country_comp(m_prd, 24, m_base, 24, i_country)
    rename!(nt_ya_to_final, [col => Symbol(string(col) * "_a") for col in cols_to_rename])

    res_country_2 = innerjoin(init_to_nt_ya, nt_ya_to_final, on=[:country, :ind], makeunique=true)

    res_country_2.tot = res_country_2.tot_t .+ res_country_2.tot_a

    @assert isapprox(res_country_2.tot, shapley_model_country(m_base, i_country).tot; rtol=1e-3)

    # Combine the two results and take average
    res_final = innerjoin(res_country, res_country_2, on=[:country, :ind], makeunique=true, renamecols="_1" => "_2")

    # Average the columns using a loop
    for suffix in ["_t", "_a"]
        for prefix in ["io", "price", "income", "nx", "tot"]
            col_name = Symbol(prefix * suffix)
            col_1 = Symbol(prefix * suffix * "_1")
            col_2 = Symbol(prefix * suffix * "_2")
            res_final[!, col_name] = (res_final[!, col_1] .+ res_final[!, col_2]) ./ 2
        end
    end

    rename!(res_final, :tot_1 => :tot)
    # Remove all columns ending in _1 or _2
    cols_to_remove = filter(name -> endswith(string(name), "_1") || endswith(string(name), "_2"), names(res_final))
    select!(res_final, Not(cols_to_remove))

    return res_final
end

res_at = vcat([a_t_decomp(i) for i in 1:N]...)

for col in (:io_t, :price_t, :income_t, :nx_t, :tot_t, :io_a, :price_a, :income_a, :nx_a, :tot_a)
    res_at[!, Symbol("r_", col)] = res_at[!, col] ./ res_at[!, :tot] .* 100
end

describe(res_at[abs.(res_at.tot).>1, :])
describe(res_at[abs.(res_at.tot).>3, :])
describe(res_at[abs.(res_at.tot).>5, :])





## Make it into a latex table
function create_summary_table(df; digits=1)
    vars = [:income_rel, :price_rel, :nx_rel, :io_rel]
    vars_name = ["Income", "Price", "Net-export", "Input-output"]

    result = DataFrame(
        Variable=String[],
        Mean=String[],
        Q1=String[],
        Q2=String[],
        Q3=String[]
    )

    for i in 1:length(vars)
        var = vars[i]
        var_name = vars_name[i]
        data = df[!, var]
        clean_data = filter(!ismissing, data)

        push!(result, (
            Variable=var_name,
            Mean=string(round(mean(clean_data), digits=digits)),
            Q1=string(round(quantile(clean_data, 0.25), digits=digits)),
            Q2=string(round(quantile(clean_data, 0.50), digits=digits)),
            Q3=string(round(quantile(clean_data, 0.75), digits=digits))
        ))
    end

    return result
end


res_1p = res[abs.(res.tot).>1, :]
tab_1p = create_summary_table(res[abs.(res.tot).>1, :], digits=1)

# Save the results: CSV, LaTeX, number of observations
CSV.write("./doc/tables/shapley_1p.csv", tab_1p)

open("./doc/tables/shapley_1p_nobs.txt", "w") do io
    write(io, "Nobs is $(nrow(res_1p)).")
end

open("./doc/tables/shapley_1p.tex", "w") do io
    pretty_table(io, tab_1p, backend=Val(:latex))
end

# For 3 percents and 5 percents
res_3p = res[abs.(res.tot).>3, :]
tab_3p = create_summary_table(res[abs.(res.tot).>3, :], digits=1)

CSV.write("./doc/tables/shapley_3p.csv", tab_3p)

open("./doc/tables/shapley_3p_nobs.txt", "w") do io
    write(io, "Nobs is $(nrow(res_3p)).")
end

open("./doc/tables/shapley_3p.tex", "w") do io
    pretty_table(io, tab_3p, backend=Val(:latex))
end

res_5p = res[abs.(res.tot).>5, :]
tab_5p = create_summary_table(res[abs.(res.tot).>5, :], digits=1)

CSV.write("./doc/tables/shapley_5p.csv", tab_5p)

open("./doc/tables/shapley_5p_nobs.txt", "w") do io
    write(io, "Nobs is $(nrow(res_5p)).")
end

open("./doc/tables/shapley_5p.tex", "w") do io
    pretty_table(io, tab_5p, backend=Val(:latex))
end





## Write the results for A and τ's.
function create_summary_table_at(df; digits=1)
    vars = [:r_income, :r_price, :r_nx, :r_io, :r_tot]
    vars_name = ["Income", "Price", "Net-export", "Input-output", "Total"]

    prims_name = ["Globalization", "Productivity growth"]
    prims = ["_t", "_a"]

    result = DataFrame(
        Primitives=String[],
        Variable=String[],
        Mean=String[],
        Q1=String[],
        Q2=String[],
        Q3=String[]
    )
    for j in 1:length(prims)
        for i in 1:length(vars)
            prim = prims[j]
            prim_name = prims_name[j]
            var = vars[i]
            var_name = vars_name[i]
            data = df[!, Symbol(var, prim)]
            clean_data = filter(!ismissing, data)

            push!(result, (
                Primitives=prim_name,
                Variable=var_name,
                Mean=string(round(mean(clean_data), digits=digits)),
                Q1=string(round(quantile(clean_data, 0.25), digits=digits)),
                Q2=string(round(quantile(clean_data, 0.50), digits=digits)),
                Q3=string(round(quantile(clean_data, 0.75), digits=digits))
            ))
        end
    end
    return result
end

res_at_1p = res[abs.(res_at.tot).>1, :]
tab_at_1p = create_summary_table_at(res_at[abs.(res_at.tot).>1, :], digits=1)

# Save the results: CSV, LaTeX, number of observations
CSV.write("./doc/tables/shapley_at_1p.csv", tab_at_1p)

open("./doc/tables/shapley_at_1p_nobs.txt", "w") do io
    write(io, "Nobs is $(nrow(res_at_1p)).")
end

open("./doc/tables/shapley_at_1p.tex", "w") do io
    pretty_table(io, tab_at_1p, backend=Val(:latex))
end

# For 3 p.p. and 5 p.p.
res_at_3p = res[abs.(res_at.tot).>3, :]
tab_at_3p = create_summary_table_at(res_at[abs.(res_at.tot).>3, :], digits=1)

CSV.write("./doc/tables/shapley_at_3p.csv", tab_at_3p)

open("./doc/tables/shapley_at_3p_nobs.txt", "w") do io
    write(io, "Nobs is $(nrow(res_at_3p)).")
end

open("./doc/tables/shapley_at_3p.tex", "w") do io
    pretty_table(io, tab_at_3p, backend=Val(:latex))
end

res_at_5p = res[abs.(res_at.tot).>5, :]
tab_at_5p = create_summary_table_at(res_at[abs.(res_at.tot).>5, :], digits=1)

CSV.write("./doc/tables/shapley_at_5p.csv", tab_at_5p)

open("./doc/tables/shapley_at_5p_nobs.txt", "w") do io
    write(io, "Nobs is $(nrow(res_at_5p)).")
end

open("./doc/tables/shapley_at_5p.tex", "w") do io
    pretty_table(io, tab_at_5p, backend=Val(:latex))
end