using vrcmort, DataFrames, CSV, Turing, JLD2


all_data = CSV.read("data/derived-data/fitting_data.csv", DataFrame; missingstring = "NA")

t0 = minimum(all_data.time[all_data.post .== 1])

all_data = all_data[all_data.cause .== 2, :]
#for now
all_data = all_data[.!ismissing.(all_data.region), :]

S = maximum(all_data.sex)
R = maximum(all_data.region[.!ismissing.(all_data.region)])
A = maximum(all_data.age)
T_pre = maximum(all_data.time[all_data.post .== 0])
T_post = maximum(all_data.time[all_data.post .== 1]) - T_pre

exposure_pre = Array{Float64}(undef, S, A, R, T_pre)
exposure_post = Array{Float64}(undef, S, A, R, T_post)
conflict_pre = Array{Float64}(undef, 1, 1, R, T_pre)
conflict_post = Array{Float64}(undef, 1, 1, R, T_post)
y_pre = Array{Int}(undef, S, A, R, T_pre)
y_post = Array{Int}(undef, S, A, R, T_post)

for s in 1:S, a in 1:A, r in 1:R
    for t in 1:T_pre
        index = findall((all_data.sex .== s) .& (all_data.age .== a) .& (all_data.region .== r) .& (all_data.time .== t))
        if s == 1 && a == 1
            conflict_pre[s, a, r, t] = all_data.conflict[index[1]]
        end
        exposure_pre[s, a, r, t] = all_data.exposure[index[1]]
        y_pre[s, a, r, t] = all_data.y[index[1]]
    end
    for t in 1:T_post
        index = findall((all_data.sex .== s) .& (all_data.age .== a) .& (all_data.region .== r) .& (all_data.time .== t + T_pre) .& (all_data.post .== 1))
        if s == 1 && a == 1
            conflict_post[s, a, r, t] = all_data.conflict[index[1]]
        end
        exposure_post[s, a, r, t] = all_data.exposure[index[1]]
        y_post[s, a, r, t] = all_data.y[index[1]]
    end
end


using LogExpFunctions, Turing, SpecialFunctions, Distributions, Random

struct VRCMortModel{S, A, R, T_pre, T_post}
    #outcomes
    y_pre::Array{Int, 4}
    y_post::Array{Int, 4}
    #inputs
    exposure_pre::Array{Float64, 4}
    exposure_post::Array{Float64, 4}
    log_exposure_pre::Array{Float64, 4}
    log_exposure_post::Array{Float64, 4}
    conflict_pre::Array{Float64, 4}
    conflict_post::Array{Float64, 4}
    #options
    region_random_effects_mortality::Bool
    region_random_effects_reporting::Bool
    under_reporting_pre::Bool
    #priors (some named tuple?)
    function VRCMortModel(
        y_pre, y_post, exposure_pre, exposure_post, conflict_pre, conflict_post;
        region_random_effects_mortality = true,
        region_random_effects_reporting = true,
        under_reporting_pre = false,
        standardize_covariates = true
    )
        S, A, R, T_pre = size(y_pre)
        T_post = size(y_post, 4)

        if standardize_covariates #need to store this post analysis
            if length(unique(conflict_pre)) > 1
                conflict_pre = (conflict_pre .- mean(conflict_pre)) ./ std(conflict_pre)
            end
            if length(unique(conflict_post)) > 1
                conflict_post = (conflict_post .- mean(conflict_post)) ./ std(conflict_post)
            end
        end

        return new{S, A, R, T_pre, T_post}(
            y_pre,
            y_post,
            exposure_pre,
            exposure_post,
            log.(exposure_pre),
            log.(exposure_post),
            conflict_pre,
            conflict_post,
            region_random_effects_mortality,
            region_random_effects_reporting,
            under_reporting_pre
        )
    end
end

struct NegativeBinomial2Log{T <: Real} <: Distributions.DiscreteUnivariateDistribution
    log_μ::Float64
    ϕ::Float64
    function NegativeBinomial2Log(log_μ::T, ϕ::T) where {T <: Real}
        new{T}(log_μ, ϕ)
    end
end

function Distributions.rand(rng::AbstractRNG, d::NegativeBinomial2Log)
    r = d.ϕ
    p = d.ϕ / (exp(d.log_μ) + d.ϕ)
    return rand(rng, NegativeBinomial(r, p))
end

function Distributions.logpdf(d::NegativeBinomial2Log, x::Int)
    log_ϕ = log(d.ϕ)
    log_ϕ_μ = logaddexp(log_ϕ, d.log_μ)

    return loggamma(x + d.ϕ) - loggamma(x + 1) - loggamma(d.ϕ) +
        d.ϕ * (log_ϕ - log_ϕ_μ) + x * (d.log_μ - log_ϕ_μ)
end

@model function negative_binomial_likelihood(; y, log_μ, ϕ)
    y ~ product_distribution(NegativeBinomial2Log.(log_μ, ϕ))
    return nothing
end

@model function mortality_regression(vrc_mort_model::VRCMortModel{S, A, R, T_pre, T_post}) where {S, A, R, T_pre, T_post}
    (; conflict_pre, conflict_post, region_random_effects_mortality) = vrc_mort_model
    #mortality parameters
    α₀ ~ Normal(-9, 2)
    αₛ ~ filldist(Normal(0, 0.5), S)
    αₐ ~ filldist(Normal(0, 1), 1, A)
    β_conf ~ filldist(truncated(Normal(0.2, 0.3); lower = 0.0), 1, 1, R)

    log_λ_pre = α₀ .+ αₛ .+ αₐ .+ (β_conf .* conflict_pre)
    log_λ_post = α₀ .+ αₛ .+ αₐ .+ (β_conf .* conflict_post)

    if region_random_effects_mortality
        uᵣ ~ to_submodel(region_random_effects(vrc_mort_model))
        log_λ_pre .+= uᵣ
        log_λ_post .+= uᵣ
    end

    return (;
        log_λ_pre = log_λ_pre,
        log_λ_post = log_λ_post
    )
end

@model function reporting_regression(vrc_mort_model::VRCMortModel{S, A, R, T_pre, T_post}) where {S, A, R, T_pre, T_post}
    (; conflict_post, region_random_effects_reporting, under_reporting_pre) = vrc_mort_model
    #reporting parameters
    κ₀ ~ Normal(2.197, 1.0) #logit(0.9)
    γ_conf ~ Normal(0, 0.3) #reporting conflict effect
    logit_ρ_post = κ₀ .+ (γ_conf .* conflict_post)

    if under_reporting_pre
        (; conflict_pre) = vrc_mort_model
        logit_ρ_pre = κ₀ .+ (γ_conf .* conflict_pre)

        #add post reporting effect
        κ_post ~ Normal(0, 0.7)
        logit_ρ_post .+= κ_post
    end

    if region_random_effects_reporting
        uᵣ ~ to_submodel(region_random_effects(vrc_mort_model))
        logit_ρ_post .+= uᵣ

        if under_reporting_pre
            logit_ρ_pre .+= uᵣ
        end
    end

    #make logscale
    log_ρ_post = loglogistic.(logit_ρ_post)

    if under_reporting_pre
        log_ρ_pre = loglogistic.(logit_ρ_pre)
    else
        log_ρ_pre = zero(κ₀)
    end

    return (;
        log_ρ_pre = log_ρ_pre,
        log_ρ_post = log_ρ_post
    )
end

@model function region_random_effects(vrc_mort_model::VRCMortModel{S, A, R, T_pre, T_post}) where {S, A, R, T_pre, T_post}
    u_raw ~ filldist(Normal(0, 1), 1, 1, R) #region random effect
    sigma_u ~ truncated(Normal(0, 0.5), lower = 0.0)
    return ((u_raw .- mean(u_raw)) .* sigma_u) 
end

@model function vrc_model(vrc_mort_model::VRCMortModel{S, A, R, T_pre, T_post}; y_pre = vrc_mort_model.y_pre, y_post = vrc_mort_model.y_post) where {S, A, R, T_pre, T_post}
#@model function vrc_model(y_pre, y_post, vrc_mort_model::VRCMortModel{S, A, R, T_pre, T_post}) where {S, A, R, T_pre, T_post}
    #dispersion
    ϕ ~ Exponential(1.0)

    mortality ~ to_submodel(mortality_regression(vrc_mort_model))
    (; log_λ_pre, log_λ_post) = mortality

    reporting ~ to_submodel(reporting_regression(vrc_mort_model))
    (; log_ρ_pre, log_ρ_post) = reporting

    #println(log_ρ_pre)
    #println(log_ρ_post)

    pre_crisis ~ to_submodel(negative_binomial_likelihood(
        y = y_pre,
        log_μ = vrc_mort_model.log_exposure_pre .+ log_λ_pre .+ log_ρ_pre,
        ϕ = ϕ
    ))

    crisis ~ to_submodel(negative_binomial_likelihood(
        y = y_post,
        log_μ = vrc_mort_model.log_exposure_post .+ log_λ_post .+ log_ρ_post,
        ϕ = ϕ
    ))
end

vrc_mort_model = VRCMortModel(
    y_pre,
    y_post,
    exposure_pre,
    exposure_post,
    conflict_pre,
    conflict_post;
    region_random_effects_mortality = true,
    region_random_effects_reporting = true,
    under_reporting_pre = false,
    standardize_covariates = true
)

model = vrc_model(vrc_mort_model)

result = sample(model, NUTS(; adtype = AutoMooncake()), 10)

names(result, :parameters)


jldsave("data/derived-data/fit_basic.jld2"; fit = fit)

load("data/derived-data/fit_basic.jld2", "fit")
