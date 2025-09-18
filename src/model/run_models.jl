## run_models.jl
## Load required packages, functions, files

using SpecialFunctions, Optim, LinearAlgebra, DataFrames, CSV, JLD2, Statistics, StatsBase

include("model.jl") # Function to solve the baseline model
include("model_nest.jl") # Function to solve the nested production model

@load "./output/model/model_input.jld2" # Model parameters and primitives

for i in 1:ncol(model_input)
    obj = names(model_input)[i]
    eval(Meta.parse(string(obj, " = model_input.", obj, "[1]")))
end

## Sanity checks: does the model provide perfect fit under the wedges approach? (with no adjustment for τ < 1 and τ infinity for BTS and CS)
fy_check = model_solve(; W_init=W_start[1], κ=0.3, max_it=1e5, N, S, L=L[1], Ā=A_wedge[1], τ=τ_orig[1], nx=nx[1], σ, ϵ, ϕ=ϕ_wedge[1], η, ρ, α=α_wedge[1], θ, ind_us, P_start=p_start[1])
println("Sanity check: in the wedges approach, model = data. The maximum gap between wages in the model and data in percentage is the following.")
@show max(abs.((fy_check.W[1] .- W_start[1]) ./ W_start[1])...) * 100





## Run the models for Data (Wedges) and Baseline

# Data = wedges approach + no adjustment in τ < 1
seq_years = 1995:2018

run_wedge(tau) = run_model(; seq_years, W_init_seq=W_start, κ=0.3, max_it=1e5, N, S, L_seq=L, Ā_seq=A_wedge, τ_seq=tau, nx_seq=nx, σ, ϵ, ϕ_seq=ϕ_wedge, η, ρ, α_seq=α_wedge, θ, ind_us, P_start_seq=p_start)

m_data = run_wedge(τ_orig)

# Baseline
run_base(tau) = run_model(; seq_years, W_init_seq=W_start, κ=0.3, max_it=1e5, N, S, L_seq=L, Ā_seq=A, τ_seq=tau, nx_seq=nx, σ, ϵ, ϕ_seq=repeat([ϕ], 24), η, ρ, α_seq=repeat([α], 24), θ, ind_us, P_start_seq=p_start)

m_base = run_base(τ)





## First set of counterfactuals: no productivity growth and no trade costs change.
m_prd = run_model(; seq_years, W_init_seq=W_start, κ=0.3, max_it=1e5, N, S, L_seq=L,
    Ā_seq=repeat([A[1]], 24),
    τ_seq=τ,
    nx_seq=nx, σ, ϵ, ϕ_seq=repeat([ϕ], 24), η, ρ, α_seq=repeat([α], 24), θ, ind_us, P_start_seq=p_start)

m_tau = run_model(; seq_years, W_init_seq=W_start, κ=0.3, max_it=1e5, N, S, L_seq=L,
    Ā_seq=A,
    τ_seq=repeat([τ[1]], 24),
    nx_seq=nx, σ, ϵ, ϕ_seq=repeat([ϕ], 24), η, ρ, α_seq=repeat([α], 24), θ, ind_us, P_start_seq=p_start)





## Second set of counterfactuals: no trade costs change
# For counterfactuals, you need sequence of trade costs fixed at initial year
fix_tau = function (τ; S, fix_inds)
    τ_fixed = []
    for i in 1:24
        t_temp = Array{Float64}(undef, N, N, S)
        for s in 1:S
            if s ∈ fix_inds
                t_temp[:, :, s] .= τ[1][:, :, s]
            else
                t_temp[:, :, s] .= τ[i][:, :, s]
            end
        end
        τ_fixed = [τ_fixed; [t_temp]]
    end
    return (τ_fixed)
end

# Counterfactual 1 (only goods liberalization)
τ_only_g = fix_tau(τ; S, fix_inds=2:3)

m_only_g = run_base(τ_only_g)

# Counterfactual 2 (only services liberalization)
τ_only_s = fix_tau(τ; S, fix_inds=[1])

m_only_s = run_base(τ_only_s)

# Counterfactual 3 (no liberalization)
τ_no = fix_tau(τ; S, fix_inds=1:3)

m_no = run_base(τ_no)





## Third set of counterfactuals: Killing the mechanisms
# Head and Ries and non-tradable services.
# Head and Ries setup. This will be the key comparison.
m_hs = run_base(τ_hs)

τ_only_g_hs = fix_tau(τ_hs; S, fix_inds=2:3)

m_only_g_hs = run_base(τ_only_g_hs)

τ_only_s_hs = fix_tau(τ_hs; S, fix_inds=[1])

m_only_s_hs = run_base(τ_only_s_hs)

τ_no_hs = fix_tau(τ_hs; S, fix_inds=1:3)

m_no_hs = run_base(τ_no_hs)

# Model with non-tradable services

# Function to change trade costs to infinity for some sectors
no_tau = function (τ, S, no_inds)
    τ_res = []
    for i in 1:24
        t_temp = Array{Float64}(undef, N, N, S)
        for s in 1:S
            if s ∈ no_inds
                t_temp[:, :, s] .= Inf
                for i in 1:N
                    t_temp[i, i, s] = 1
                end
            else
                t_temp[:, :, s] .= τ[i][:, :, s]
            end
        end
        τ_res = [τ_res; [t_temp]]
    end
    return (τ_res)
end

run_no_s_trade(tau) = run_model(; seq_years, W_init_seq=W_start, κ=0.3, max_it=1e5, N, S, L_seq=L, Ā_seq=A_no_s_trade, τ_seq=tau, nx_seq=repeat([zeros(67)], 24), σ, ϵ, ϕ_seq=repeat([ϕ], 24), η, ρ, α_seq=repeat([α], 24), θ, ind_us, P_start_seq=p_start) # Instead of nx_seq = nx, I had to use zeros. Refer to the paper for detailed reasoning.

τ_no_s_trade = no_tau(τ, S, 2:3)

τ_no_s_trade_no = fix_tau(τ_no_s_trade; S, fix_inds=1:3)

m_no_s_tr = run_no_s_trade(τ_no_s_trade) # No services trade, good globalization

m_no_s_tr_no = run_no_s_trade(τ_no_s_trade_no) # No services, trade, no goods globalization





## Fourth set of counterfactuals: interactions between productivities and trade costs.
run_interaction(tau) = run_model(; seq_years, W_init_seq=W_start, κ=0.3, max_it=1e5, N, S, L_seq=L,
    Ā_seq=A_interaction,
    τ_seq=tau, nx_seq=nx, σ, ϵ, ϕ_seq=repeat([ϕ], 24), η, ρ, α_seq=repeat([α], 24), θ, ind_us, P_start_seq=p_start)

m_interaction = run_interaction(τ)

m_interaction_no = run_interaction(τ_no)





## Robustness for the main exercise: to compare (globalization bias index) vs (baseline - counterfactual 3)

# Wedges approach
m_wedge = m_data

τ_no_orig = fix_tau(τ; S, fix_inds=1:3)

m_wedge_no = run_wedge(τ_no_orig)

# Alternative three sector definition

run_alt(tau) = run_model(; seq_years, W_init_seq=W_start, κ=0.3, max_it=1e5, N, S, L_seq=L, Ā_seq=A_alt, τ_seq=tau, nx_seq=nx, σ=σ_alt, ϵ=ϵ_alt, ϕ_seq=repeat([ϕ_alt], 24), η, ρ=ρ_alt, α_seq=repeat([α_alt], 24), θ, ind_us, P_start_seq=p_start)

m_alt = run_alt(τ_alt)

τ_alt_no = fix_tau(τ_alt; S, fix_inds=1:3)

m_alt_no = run_alt(τ_alt_no)

# Two sector (G and S)

run_gs(tau) = run_model(; seq_years, W_init_seq=W_start, κ=0.3, max_it=1e5, N, S=S_gs, L_seq=L, Ā_seq=A_gs, τ_seq=tau, nx_seq=nx, σ=σ_gs, ϵ=ϵ_gs, ϕ_seq=repeat([ϕ_gs], 24), η=η_gs, ρ=ρ_gs, α_seq=repeat([α_gs], 24), θ=θ_gs, ind_us, P_start_seq=p_start_gs)

m_gs = run_gs(τ_gs)

τ_gs_no = fix_tau(τ_gs; S=S_gs, fix_inds=1:2)

m_gs_no = run_gs(τ_gs_no)

# Low theta for services & high theta for services

run_low(tau) = run_model(; seq_years, W_init_seq=W_start, κ=0.3, max_it=1e5, N, S, L_seq=L, Ā_seq=A_low, τ_seq=tau, nx_seq=nx, σ, ϵ, ϕ_seq=repeat([ϕ], 24), η, ρ, α_seq=repeat([α], 24), θ=θ_low, ind_us, P_start_seq=p_start)
run_high(tau) = run_model(; seq_years, W_init_seq=W_start, κ=0.3, max_it=1e5, N, S, L_seq=L, Ā_seq=A_high, τ_seq=tau, nx_seq=nx, σ, ϵ, ϕ_seq=repeat([ϕ], 24), η, ρ, α_seq=repeat([α], 24), θ=θ_high, ind_us, P_start_seq=p_start)

m_low = run_low(τ_low)

τ_low_no = fix_tau(τ_low; S, fix_inds=1:3)

m_low_no = run_low(τ_low_no)

m_high = run_high(τ_high)

τ_high_no = fix_tau(τ_high; S, fix_inds=1:3)

m_high_no = run_high(τ_high_no)

# 6. Nested version

run_nest(tau) = run_model_nest(; seq_years, W_init_seq=W_start, κ=0.3, max_it=1e5, N, S, L_seq=L, Ā_seq=A_nest, τ_seq=tau, nx_seq=nx, σ, ϵ, ϕ_seq=repeat([ϕ], 24), η, ρ_inner, ρ_outer, α_inner_seq=repeat([α_inner], 24), α_outer_seq=repeat([α_outer], 24), θ=θ, ind_us, P_start_seq=p_start)

m_nest = run_nest(τ)

m_nest_no = run_nest(τ_no)

# No NX

run_nx(tau) = run_model(; seq_years, W_init_seq=W_start, κ=0.3, max_it=1e5, N, S, L_seq=L, Ā_seq=A, τ_seq=tau, nx_seq=repeat([zeros(67)], 24), σ, ϵ, ϕ_seq=repeat([ϕ], 24), η, ρ, α_seq=repeat([α], 24), θ, ind_us, P_start_seq=p_start)

m_nx = run_nx(τ)

m_nx_no = run_nx(τ_no)

# No BTS trade

τ_bts = no_tau(τ, 3, [2])

τ_bts_no = no_tau(τ_no, 3, [2])

m_bts = run_base(τ_bts)

m_bts_no = run_base(τ_bts_no)

# No adjustment for τ < 1, and τ BTS infty issue

m_orig = run_base(τ_orig)

τ_no_orig = fix_tau(τ_orig; S, fix_inds=1:3)

m_orig_no = run_base(τ_no_orig)


# Save the result
jldsave("./output/model/models.jld2";
    m_data, m_base, # Data & baseline
    m_prd, m_tau, # Killing prd and tau
    m_only_g, m_only_s, m_no, # Counterfactuals 1, 2, 3
    m_hs, m_only_g_hs, m_only_s_hs, m_no_hs, # H-S
    m_no_s_tr, m_no_s_tr_no, # No services trade
    m_interaction, m_interaction_no, # Interaction between prd and trade costs
    m_wedge, m_wedge_no, # Robustness: wedges
    m_alt, m_alt_no, # Rob: alternative three sector definition
    m_gs, m_gs_no, # Rob: two sector (G and S)
    m_low, m_low_no, # Rob: low theta for services & high theta for services
    m_high, m_high_no,
    m_nest, m_nest_no, # Rob: nested CES production function
    m_nx, m_nx_no, # Rob: no NX
    m_bts, m_bts_no, # Rob: no BTS trade
    m_orig, m_orig_no) # Rob: original trade costs (no adjustments for below-one and infty trade costs)

print("Code ran without a problem")