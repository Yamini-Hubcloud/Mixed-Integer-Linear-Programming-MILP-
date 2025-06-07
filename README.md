# Mixed-Integer-Linear-Programming-MILP
Laptop Manufacturing Optimization Framework
1. Introduction
This analysis focuses on optimizing laptop production over a 26-week planning horizon. The objective is to minimize total operational and setup costs while fulfilling weekly pre-order demand, with and without a 20% buffer for additional production capacity.

The decision-making process is modeled using a Mixed-Integer Programming (MIP) approach, accounting for:

Factory setup costs (one-time),

Minimum and maximum production capacities per factory class,

Weekly fixed and variable production costs,

Weekly demand forecasts.

Three factory classes, each with different cost and capacity profiles, are considered. The model determines the optimal number of factories to operate per week and their respective production levels to meet demand in the most cost-effective way.

2. Methodology
The optimization was implemented in R using the ompr and ROI packages with the GLPK solver.

Two scenarios were evaluated:

Scenario A: Meeting only the forecast demand.

Scenario B: Providing a 20% buffer for potential demand increases.

Key constraints:

Production must lie within min/max capacity per factory.

Weekly production must meet or exceed demand (with or without buffer).

Factory activation is bounded by availability limits.

The model was solved for both scenarios, and results were compared across total cost, factory usage, and production profiles.
