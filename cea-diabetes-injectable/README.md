# Cost-Effectiveness of a Once-Weekly Injectable for Type 2 Diabetes in Sweden

This project evaluates the cost-effectiveness of a hypothetical once-weekly injectable therapy for adults with type 2 diabetes in Sweden. Using simulated data and R-based analysis, the project applies real-world health economic methods to assess incremental costs, QALYs, and uncertainty.

## Methods

- Perspective: Healthcare & societal
- R-based bootstrapping (1,000 iterations)
- Subgroup analysis (e.g., age, gender)
- Cost-effectiveness acceptability curve (CEAC)
- One-way sensitivity analysis (OWSA)
- WTP threshold: 500,000 SEK/QALY

## Key Results (Simulated Data)

| Perspective | ICER (SEK/QALY) | Incremental Cost | Incremental QALY |
|-------------|-----------------|------------------|------------------|
| Healthcare  | 50,739          | 542 SEK          | 0.0107           |
| Societal    | -101,603        | -1,086 SEK       | 0.0107           |

- Subgroup ICERs varied by sex and age
- Bootstrapped QALY 95% CI = [-0.0294, 0.0541]
- CEAC shows 72% probability of cost-effectiveness at 500,000 SEK threshold

## Files

- `R-script.R` – Full model code
- `report.md` – Project summary
- `CE_Plane_Trimmed_Final.png`, `CEAC_final.png`, `OWSA_Final.png` – Visual outputs

## Takeaways

- Demonstrates technical competence in economic modeling
- Strong focus on uncertainty and real-world relevance
- Applicable to market access, HEOR, and early pipeline evaluations
