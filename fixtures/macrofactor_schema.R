# MacroFactor export schema: sheet and column names only (no values).
# Taken once from the headers of a real export (Nov 2026).
# Used by fixtures/make_macrofactor_fixture.R. If MacroFactor changes its
# export, update this file and the loaders together.

macrofactor_schema <- list(
  micronutrients = c("Alcohol (g)", "B12, Cobalamin (mcg)", "B1, Thiamine (mg)", "B2, Riboflavin (mg)", 
    "B3, Niacin (mg)", "B5, Pantothenic Acid (mg)", "B6, Pyridoxine (mg)", 
    "Caffeine (mg)", "Calcium (mg)", "Cholesterol (mg)", "Choline (mg)", "Copper (mg)", 
    "Cysteine (g)", "Monounsaturated Fat (g)", "Polyunsaturated Fat (g)", "Saturated Fat (g)", 
    "Trans Fat (g)", "Fiber (g)", "Folate (mcg)", "Histidine (g)", "Iron (mg)", 
    "Isoleucine (g)", "Leucine (g)", "Lysine (g)", "Magnesium (mg)", "Manganese (mg)", 
    "Methionine (g)", "Omega-3 ALA (g)", "Omega-3 DHA (g)", "Omega-3 EPA (g)", 
    "Omega-3 (g)", "Omega-6 (g)", "Phenylalanine (g)", "Phosphorus (mg)", "Potassium (mg)", 
    "Selenium (mcg)", "Sodium (mg)", "Starch (g)", "Sugars (g)", "Sugars Added (g)", 
    "Threonine (g)", "Tryptophan (g)", "Tyrosine (g)", "Valine (g)", "Vitamin A (mcg)", 
    "Vitamin C (mg)", "Vitamin D (mcg)", "Vitamin E (mg)", "Vitamin K (mcg)", 
    "Water (g)", "Zinc (mg)"),
  body_metrics = c("Bust (cm)", "Chest (cm)", "Hips (cm)", "Left Ankle (cm)", "Left Bicep (cm)", 
    "Left Calf (cm)", "Left Forearm (cm)", "Left Thigh (cm)", "Left Wrist (cm)", 
    "Neck (cm)", "Right Ankle (cm)", "Right Bicep (cm)", "Right Calf (cm)", 
    "Right Forearm (cm)", "Right Thigh (cm)", "Right Wrist (cm)", "Shoulders (cm)", 
    "Visual Body Fat Assessment", "Waist (cm)"),
  muscle_groups = c("Chest", "Quads", "Upper Back", "Glutes", "Lats", "Hamstrings", "Biceps", 
    "Triceps", "Front Delts", "Side Delts", "Lower Back", "Abs", "Calves", 
    "Upper Traps", "Rear Delts", "Forearms", "Obliques", "Abductors", "Adductors", 
    "Neck", "Tibialis", "Serratus"),
  food_log = c("Date", "Time", "Food Name", "Serving Size", "Serving Qty", "Serving Weight (g)", 
    "Calories (kcal)", "Fat (g)", "Carbs (g)", "Protein (g)", "Alcohol (g)", 
    "B12, Cobalamin (mcg)", "B1, Thiamine (mg)", "B2, Riboflavin (mg)", "B3, Niacin (mg)", 
    "B5, Pantothenic Acid (mg)", "B6, Pyridoxine (mg)", "Caffeine (mg)", "Calcium (mg)", 
    "Cholesterol (mg)", "Choline (mg)", "Copper (mg)", "Cysteine (g)", "Monounsaturated Fat (g)", 
    "Polyunsaturated Fat (g)", "Saturated Fat (g)", "Trans Fat (g)", "Fiber (g)", 
    "Folate (mcg)", "Histidine (g)", "Iron (mg)", "Isoleucine (g)", "Leucine (g)", 
    "Lysine (g)", "Magnesium (mg)", "Manganese (mg)", "Methionine (g)", "Omega-3 ALA (g)", 
    "Omega-3 DHA (g)", "Omega-3 EPA (g)", "Omega-3 (g)", "Omega-6 (g)", "Phenylalanine (g)", 
    "Phosphorus (mg)", "Potassium (mg)", "Selenium (mcg)", "Sodium (mg)", "Starch (g)", 
    "Sugars (g)", "Sugars Added (g)", "Threonine (g)", "Tryptophan (g)", "Tyrosine (g)", 
    "Valine (g)", "Vitamin A (mcg)", "Vitamin C (mg)", "Vitamin D (mcg)", "Vitamin E (mg)", 
    "Vitamin K (mcg)", "Water (g)", "Zinc (mg)"),
  workout_log = c("Date", "Workout Duration", "Workout", "Exercise", "Exercise Base Weight (kg)", 
    "Set Type", "Weight (kg)", "Reps", "RIR", "Duration", "Distance short (m)", 
    "Distance long (km)")
)
