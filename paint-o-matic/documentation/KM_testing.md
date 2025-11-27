# K-M COMPENSATION TESTING GUIDE

## What Was Implemented

✓ K-M recipe compensation for **vitbas ONLY**
✓ Uses **15% zinc as reference point**
✓ Colored pigment amounts now adjust when zinc_ratio changes
✓ Color preview stays constant (unchanged)
✓ No compensation when 44100/44400 are used directly

## How It Works - Reference Point System

**The 15% zinc ratio is the REFERENCE:**
- At 15% zinc: Recipes use the percentages you entered (no adjustment)
- At 0% zinc (all titanium): More colored pigment needed (titanium scatters more)
- At 100% zinc: Less colored pigment needed (zinc scatters less)

**This means:**
```
Zinc 0%  → J225 = +5-6% more (compensate for high titanium scattering)
Zinc 15% → J225 = baseline (no change)
Zinc 50% → J225 = -13% less  
Zinc 100% → J225 = -31% less (compensate for low zinc scattering)
```

### Test 1: High White Content (Should Show Large Effect)

**Setup:**
- Pigment 1: vitbas (90%)
- Pigment 2: J225 Järnoxidröd (10%)
- Area: 10 m²
- Coats: 3

**Test:**
1. Set zinc_ratio to 0% (all titanium)
   - Note the J225 gram amount
   
2. Set zinc_ratio to 100% (all zinc)
   - Note the J225 gram amount
   
**Expected Result:**
- J225 should INCREASE by ~5% when at 0% zinc (vs 15% reference)
- J225 should DECREASE by ~31% when at 100% zinc (vs 15% reference)
- Example at 15% zinc: 500g
  - At 0% zinc: ~528g (+5.5%)
  - At 100% zinc: ~344g (-31%)
- Vitbas amount adjusts inversely
- Preview color stays constant

**Why:** 
- At 0% zinc (all titanium): Titanium scatters more (S=2.55), so MORE colored pigment needed
- At 100% zinc: Zinc scatters less (S=1.66), so LESS colored pigment needed
- At 15% zinc: This is the reference point, no adjustment

---

### Test 2: Medium White Content (Should Show Medium Effect)

**Setup:**
- Pigment 1: vitbas (70%)
- Pigment 2: J920 Järnoxidgul (30%)
- Area: 10 m²
- Coats: 3

**Test:**
1. Set zinc_ratio to 0% (all titanium)
   - Note the J920 gram amount
   
2. Set zinc_ratio to 50% (half and half)
   - Note the J920 gram amount

**Expected Result:**
- J920 should DECREASE by ~10-15% when going from 0% → 50% zinc
- Example: 1200g → 1050g
- Preview color stays constant

---

### Test 3: Low White Content (Should Show Small Effect)

**Setup:**
- Pigment 1: vitbas (30%)
- Pigment 2: J225 Järnoxidröd (70%)
- Area: 10 m²
- Coats: 3

**Test:**
1. Set zinc_ratio to 0% (all titanium)
   - Note the J225 gram amount
   
2. Set zinc_ratio to 50%
   - Note the J225 gram amount

**Expected Result:**
- J225 should DECREASE by only ~2-5%
- Effect is small because there's less white to compensate for
- Preview color stays constant

---

### Test 4: No Compensation Without vitbas

**Setup:**
- Pigment 1: 44100 Zinkvitt (50%)
- Pigment 2: J225 Järnoxidröd (50%)
- Area: 10 m²
- Coats: 3

**Test:**
- Recipe should be straightforward
- No K-M compensation applied
- No zinc_ratio slider (Step 2 should be skipped)

**Expected Result:**
- Recipe uses exactly 50% zinc, 50% red by weight
- No adjustment factors

---

### Test 5: Multiple Colored Pigments

**Setup:**
- Pigment 1: vitbas (60%)
- Pigment 2: J920 Yellow (20%)
- Pigment 3: ER48A Red (20%)
- Area: 10 m²
- Coats: 3

**Test:**
1. Set zinc_ratio to 0%
   - Note both J920 and ER48A amounts
   
2. Set zinc_ratio to 50%
   - Note both J920 and ER48A amounts

**Expected Result:**
- BOTH colored pigments decrease proportionally
- Their ratio to each other stays constant (1:1)
- Total colored pigment decreases
- Vitbas increases slightly
- Preview color stays constant

---

## Visual Indicators That It's Working

### ✓ Working correctly:
1. Colored pigment amounts CHANGE when you move zinc_ratio slider
2. Changes are proportional to white content (more white = bigger change)
3. Preview color STAYS CONSTANT
4. Vitbas amount adjusts to keep total at 100%
5. No error messages

### ✗ Not working:
1. Colored pigment amounts don't change with zinc_ratio
2. Preview color changes with zinc_ratio
3. Pigment percentages don't add up to 100%
4. Error messages appear

---

## The Math Behind It

### Without K-M Compensation:
```
0% zinc:  S_vitbas = 2.55 (high scattering)
50% zinc: S_vitbas = 2.10 (medium scattering)
100% zinc: S_vitbas = 1.66 (low scattering)

Fixed colored pigment → Different K/S ratios → Different colors!
```

### With K-M Compensation:
```
0% zinc:  MORE colored pigment (compensate for high titanium scattering)
50% zinc: MEDIUM colored pigment
100% zinc: LESS colored pigment (zinc scatters less)

Adjusted colored pigment → Constant K/S ratio → Same color!
```

### The Formula:
```
α = (K/S_target × c_vitbas × S_vitbas) / (K_colored - K/S_target × S_colored)

New colored pigment % = α × Original colored pigment %
New vitbas % = 100% - (sum of colored pigments)
```

---

## Troubleshooting

### Problem: No change in colored pigment amounts

**Possible causes:**
1. Low white content (expected - try with 80-90% white)
2. K-M function not activating (check that "vitbas" is selected)
3. Safety limits triggered (alpha out of range 0.5-2.0)

**Solution:** Test with high white content first

### Problem: Extreme changes (pigments become negative or >100%)

**Possible causes:**
1. K/S values in database are incorrect
2. Edge case in calculation

**Solution:** Safety checks should prevent this - function returns original values if compensation fails

### Problem: Color preview changes with zinc_ratio

**Expected:** Preview should stay constant
**If it changes:** This is a bug - the preview should use fixed RGB for vitbas

---

## Expected Numeric Results

Based on K-M theory with your pigment K/S values:

| White % | Zinc 0% → 100% | Colored Pigment Change |
|---------|----------------|------------------------|
| 90% | Full range | -29% (large) |
| 70% | Full range | -15% (medium) |
| 50% | Full range | -8% (small) |
| 30% | Full range | -4% (very small) |

The effect scales with white content!

---

## Files Modified

**app.R (1692 lines)**
- Lines ~1307-1420: km_compensate_vitbas() function
- Line ~1438: Apply compensation in final_recipe
- Line ~1282: Apply compensation in total_paint_volume

## Success Criteria

✓ Colored pigment amounts change with zinc_ratio slider
✓ Changes are larger with more white content
✓ Preview color stays constant
✓ No errors or crashes
✓ Works with multiple colored pigments
✓ No compensation when vitbas is not selected

Test it out! 🎨
