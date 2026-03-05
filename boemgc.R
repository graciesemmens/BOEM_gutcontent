# ============================================================
# BOEM Gut Content Analysis
# ============================================================
#check check 

# ------------------------------------------------------------
# upkeep
# ------------------------------------------------------------
library(tidyverse)


# ============================================================
# 1. load data
# ============================================================
df_raw <- read.csv("BOEM_GC_Analysis(Sheet1).csv")

glimpse(df_raw)
#bc my computer is being ridiculous 
df_raw$Fish.Species[df_raw$Fish.ID == "LIZ020"]   <- "Lizardfish"
df_raw$Fish.Species[df_raw$Fish.ID == "ATB046gc"]  <- "Atlantic Bumper"
df_raw$Fish.Species[df_raw$Fish.ID == "PIN 173"]   <- "Pinfish"


# ============================================================
# 2. data cleanin
# ============================================================
df <- df_raw %>%
  rename(
    project          = Project,
    dissection_date  = Disection.Date,
    dissector        = Name,
    fish_species     = Fish.Species,
    fish_id          = Fish.ID,
    stomach_id       = Stomach.ID,
    site             = Site,
    collection_date  = Collection.Date,
    prey_common_name = Prey.Common.Name,
    prey_wet_weight  = Prey.Wet.Weight..g.,
    prey_category    = Prey,
    n_prey           = X..Prey,
    prey_condition   = Prey.Conditions..1.5.,
    notes            = Notes
  )


# ============================================================
# 3. DROP ROWS WHERE PREY HASN'T BEEN ENTERED YET
#
# some rows have a blank Prey column because the data is still
# being updated. this removes them for now so they don't interfere
# with any analyses. check for: actual NA, empty string "",
# or whitespace-only strings like "  ".

# ============================================================
cat("Rows before removing blank prey:", nrow(df), "\n")

df <- df %>%
  mutate(
    # Trim whitespace first, then convert empty strings to NA
    prey_category = str_trim(prey_category),
    prey_category = na_if(prey_category, "")
  ) %>%
  # Now drop any row where prey_category is NA (blank/not entered)
  filter(!is.na(prey_category))

cat("Rows after removing blank prey: ", nrow(df), "\n")
cat("Rows dropped:                   ", nrow(df_raw) - nrow(df), "\n")


# ============================================================
# 4. handling multi-prey stomachs
#
# if a stomach contains multiple different prey items they
# are recorded as separate rows with the same fish_id but
# different stomach_id suffixes: gc1, gc2, gc3 etc.
# for example:
#   PIN082  PIN082gc1  shell
#   PIN082  PIN082gc2  anchovy
# These are both from the same individual fish PIN082.
#
# create a clean base_fish_id
# column that strips the gc1/gc2/gc3 suffix from stomach_id
# and just uses fish_id directly - this is already the unique
# individual identifier we need.
#
# The stomach_id column (gc1/gc2/gc3) just tells us it's a
# separate prey item from the same stomach - we keep it for
# reference but use fish_id for all grouping operations.
#
# ============================================================
df <- df %>%
  mutate(
    # fish_id is already the individual fish identifier
    # we just clean it up (trim whitespace) to be safe
    fish_id = str_trim(fish_id),
    
    # Create a clean individual ID by also stripping any
    # trailing numbers from stomach_id just in case
    # (e.g. "PIN082gc1" --> "PIN082gc", same individual as "PIN082gc2")
    stomach_id_base = str_remove(stomach_id, "[0-9]+$")
  )

# Verify: show an example of a multi-prey individual
# You should see the same fish_id appearing multiple times
df %>%
  filter(fish_id == "PIN082") %>%
  select(fish_id, stomach_id, stomach_id_base, prey_category)


# ============================================================
# 5. cleaning up weights 
#
# weight column contains mixed values:
#   "<0.1"  = below detection limit --> substitute 0.05
#             (half the detection limit, standard practice)
#   "<0.2"  = same logic            --> substitute 0.1
#   ">0.1"  = above threshold       --> substitute 0.1
#   "-"     = missing/not recorded  --> NA
#   numbers = actual measurements   --> keep as-is
#
# for now, i am using half the stated limit for all "<" values, e.g.:
#   <0.1 --> 0.05, <0.2 --> 0.1, <0.3 --> 0.15
# ============================================================
df <- df %>%
  mutate(
    weight_raw = str_trim(as.character(prey_wet_weight)),
    prey_wet_weight_g = case_when(
      # Strip "<" and divide by 2 (half the detection limit)
      str_detect(weight_raw, "^<")       ~ as.numeric(str_remove(weight_raw, "<")) / 2,
      # Strip ">" and use value as-is
      str_detect(weight_raw, "^>")       ~ as.numeric(str_remove(weight_raw, ">")),
      # Dashes = genuinely missing data
      weight_raw %in% c("-", " - ", "") ~ NA_real_,
      # Everything else: convert directly to numeric
      TRUE ~ suppressWarnings(as.numeric(weight_raw))
    )
  ) %>%
  select(-weight_raw)

# Sanity check "-"
df %>%
  filter(is.na(prey_wet_weight_g)) %>%
  count(prey_wet_weight)


# ============================================================
# 6. standardizing prey names (shouldve done this before but whatever)
#
# The Prey column has inconsistent naming due to:
#   - trailing spaces ("crustacean " vs "crustacean")
#   - typos ("crustcacean", "polycheate", "biomattter")
#   - multiple names for the same thing
#
# ============================================================
df <- df %>%
  mutate(
    prey_category_clean = str_trim(tolower(prey_category)),
    prey_category_clean = case_when(
      prey_category_clean %in% c("crustcacean", "crustcean", "crustacean based biomatter") ~ "crustacean",
      prey_category_clean %in% c("mollusc", "molusca", "squid (mollusca)")                ~ "mollusca",
      prey_category_clean %in% c("biomattter", "biomatter, very weird bone thing")        ~ "biomatter",
      prey_category_clean == "polycheate"                                                  ~ "polychaete",
      prey_category_clean == "detritus + maybe a shell"                                    ~ "detritus",
      prey_category_clean == "unidentifiable fish?"                                        ~ "unidentifiable fish",
      prey_category_clean == "striped anchovy"                                             ~ "anchovy",
      TRUE ~ prey_category_clean
    )
  )

# Verify - should now show only clean category names
df %>% count(prey_category_clean, sort = TRUE)

# ============================================================
# 7. more cleaning + remove nas 
#
# A few rows have "Not found" as the species - these are fish
# that couldn't be identified, so we remove them.
# ============================================================
df <- df %>%
  mutate(
    fish_species = str_trim(fish_species),
    fish_species = na_if(fish_species, "Not found")
  )

# Count NAs in key columns before dropping
df %>% summarise(
  na_species   = sum(is.na(fish_species)),
  na_weight    = sum(is.na(prey_wet_weight_g)),
  na_category  = sum(is.na(prey_category_clean)),
  na_site      = sum(is.na(site))
)

# Drop rows with unknown fish species
df_clean <- df %>%
  filter(!is.na(fish_species))

# Flag rows with/without valid weight
# We keep both - rows without weights still count for FOO
df_clean <- df_clean %>%
  mutate(has_weight = !is.na(prey_wet_weight_g))

cat("Total rows after cleaning:", nrow(df_clean), "\n")
cat("Rows with valid weights:  ", sum(df_clean$has_weight), "\n")
cat("Unique fish species:      ", n_distinct(df_clean$fish_species), "\n")
cat("Unique individual fish:   ", n_distinct(df_clean$fish_id), "\n")


# ============================================================
# 8. site codes
#
# Site codes are [E/W][I/O][1/2/3]:
#   
# ============================================================
df_clean <- df_clean %>%
  mutate(
    site     = str_trim(toupper(site)),
    region   = case_when(
      str_starts(site, "E") ~ "East",
      str_starts(site, "W") ~ "West",
      TRUE ~ NA_character_
    ),
    position = case_when(
      str_detect(site, "I") ~ "Inner",
      str_detect(site, "O") ~ "Outer",
      TRUE ~ NA_character_
    ),
    station  = str_extract(site, "[0-9]+")
  )


# ============================================================
# 9. FREQUENCY OF OCCURRENCE (FOO) - ALL PREY
#
# FOO = the % of individual fish stomachs containing each prey
# This is the most robust diet metric when weights are uncertain
#
# IMPORTANT: We group by fish_id (the individual fish), NOT
# stomach_id. This means if PIN082 had both shell (gc1) and
# anchovy (gc2), it counts as ONE stomach that contained BOTH
# shell and anchovy - which is correct.
#
# How the calculation works:
#   1. For each fish_id, collect all unique prey categories
#   2. Count how many fish had each prey category
#   3. Divide by total fish per species x 100
# ============================================================
# Total stomachs per species (denominator for FOO)
n_stomachs <- df_clean %>%
  group_by(fish_species) %>%
  summarise(n_stomachs_total = n_distinct(fish_id), .groups = "drop")

foo <- df_clean %>%
  filter(!is.na(prey_category_clean)) %>%
  # Group by INDIVIDUAL FISH (fish_id) to handle multi-prey stomachs correctly
  group_by(fish_species, fish_id) %>%
  # For each fish, get the unique set of prey categories found
  summarise(prey_cats = list(unique(prey_category_clean)), .groups = "drop") %>%
  # Expand back out so each prey category is its own row
  unnest(prey_cats) %>%
  # Count how many fish per species had each prey category
  group_by(fish_species, prey_cats) %>%
  summarise(n_stomachs_with_prey = n(), .groups = "drop") %>%
  left_join(n_stomachs, by = "fish_species") %>%
  mutate(FOO_percent = (n_stomachs_with_prey / n_stomachs_total) * 100) %>%
  arrange(fish_species, desc(FOO_percent))

print(foo, n = 60, width = Inf)


# ============================================================
# 10. FOO - IDENTIFIABLE PREY ONLY
#
# Same as above but excluding:
#   - Unidentifiable Mush (too degraded)
#   - Biomatter           (too degraded)
#   - Detritus            (non-prey material like sand/sediment)
#
# The denominator stays as ALL stomachs so percentages are
# still directly comparable across species.
# ============================================================
unidentifiable <- c("unidentifiable mush", "biomatter", "detritus")

foo_id <- df_clean %>%
  filter(
    !is.na(prey_category_clean),
    !prey_category_clean %in% unidentifiable
  ) %>%
  group_by(fish_species, fish_id) %>%
  summarise(prey_cats = list(unique(prey_category_clean)), .groups = "drop") %>%
  unnest(prey_cats) %>%
  group_by(fish_species, prey_cats) %>%
  summarise(n_stomachs_with_prey = n(), .groups = "drop") %>%
  # Still use FULL stomach count as denominator
  left_join(n_stomachs, by = "fish_species") %>%
  mutate(FOO_percent = (n_stomachs_with_prey / n_stomachs_total) * 100) %>%
  arrange(fish_species, desc(FOO_percent))

cat("\n--- FOO: Identifiable Prey Only ---\n")
print(foo_id, n = 60, width = Inf)


# ============================================================
# 11. PREY WEIGHT SUMMARY BY SPECIES + CATEGORY
# ============================================================
weight_summary <- df_clean %>%
  filter(has_weight, !is.na(prey_category_clean)) %>%
  group_by(fish_species, prey_category_clean) %>%
  summarise(
    mean_weight_g   = mean(prey_wet_weight_g, na.rm = TRUE),
    median_weight_g = median(prey_wet_weight_g, na.rm = TRUE),
    total_weight_g  = sum(prey_wet_weight_g, na.rm = TRUE),
    n               = n(),
    .groups = "drop"
  ) %>%
  arrange(fish_species, desc(total_weight_g))

print(weight_summary, n = 60, width = Inf)


# ============================================================
# 12. PLOTS
# ============================================================

prey_colors <- c(
  "unidentifiable mush" = "grey70",
  "biomatter"           = "grey40",
  "crustacean"          = "#2196F3",
  "anchovy"             = "#FF5722",
  "unidentifiable fish" = "#FF9800",
  "polychaete"          = "#9C27B0",
  "mollusca"            = "#4CAF50",
  "detritus"            = "#795548",
  "echinoderm"          = "#E91E63"
)

# --- Plot 1: FOO stacked bar - mush excluded ---
p1 <- foo %>%
  filter(prey_cats != "Unidentifiable Mush") %>%
  ggplot(aes(
    x    = reorder(fish_species, -n_stomachs_total),
    y    = FOO_percent,
    fill = prey_cats
  )) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = prey_colors) +
  labs(
    title = "Frequency of Occurrence by Fish Species (mush excluded)",
    x     = NULL, y = "% of Stomachs", fill = "Prey Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)
ggsave("FOO_by_species.png", p1, width = 11, height = 6, dpi = 300)


# --- Plot 2: Identifiable prey only FOO ---
p2 <- foo_id %>%
  ggplot(aes(
    x    = reorder(fish_species, -n_stomachs_total),
    y    = FOO_percent,
    fill = prey_cats
  )) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = prey_colors) +
  labs(
    title    = "Frequency of Occurrence - Identifiable Prey Only",
    subtitle = "Mush, biomatter, and detritus excluded",
    x = NULL, y = "% of Stomachs", fill = "Prey Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)
ggsave("FOO_identifiable_only.png", p2, width = 11, height = 6, dpi = 300)


# --- Plot 3: Proportional diet composition - all categories ---
# Recalculate proportions properly by dividing each species' FOO by its total
p3 <- foo %>%
  group_by(fish_species) %>%
  mutate(prop = FOO_percent / sum(FOO_percent)) %>%
  ungroup() %>%
  ggplot(aes(
    x    = reorder(fish_species, -n_stomachs_total),
    y    = prop,
    fill = prey_cats
  )) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = prey_colors) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportional Diet Composition by Species (all categories)",
    x = NULL, y = "Proportion of Stomachs", fill = "Prey Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)
ggsave("diet_composition_proportional.png", p3, width = 11, height = 6, dpi = 300)


# --- Plot 4: Identifiable prey FOO - stacked bar with actual % values ---
p4_id <- foo_id %>%
  ggplot(aes(
    x    = reorder(fish_species, -n_stomachs_total),
    y    = FOO_percent,
    fill = prey_cats
  )) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = prey_colors) +
  labs(
    title    = "Frequency of Occurrence - Identifiable Prey Only",
    subtitle = "Mush, biomatter, and detritus excluded",
    x = NULL, y = "% of Stomachs", fill = "Prey Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p4_id)
ggsave("FOO_identifiable_only.png", p4_id, width = 11, height = 6, dpi = 300)


# --- Plot 5: FOO by East vs West region ---
# facet_wrap() makes a separate panel per species
foo_region <- df_clean %>%
  filter(!is.na(region), !is.na(prey_category_clean)) %>%
  group_by(fish_species, region, fish_id) %>%
  summarise(prey_cats = list(unique(prey_category_clean)), .groups = "drop") %>%
  unnest(prey_cats) %>%
  group_by(fish_species, region, prey_cats) %>%
  summarise(n_with = n(), .groups = "drop") %>%
  left_join(
    df_clean %>%
      filter(!is.na(region)) %>%
      group_by(fish_species, region) %>%
      summarise(n_total = n_distinct(fish_id), .groups = "drop"),
    by = c("fish_species", "region")
  ) %>%
  mutate(FOO = (n_with / n_total) * 100)

p5 <- foo_region %>%
  filter(!prey_cats %in% unidentifiable) %>%
  ggplot(aes(x = prey_cats, y = FOO, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~fish_species, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("East" = "#E64B35", "West" = "#4DBBD5")) +
  labs(
    title    = "Diet by Region (East vs West) per Species",
    subtitle = "Identifiable prey only",
    x = "Prey Category", y = "Frequency of Occurrence (%)", fill = "Region"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text  = element_text(face = "bold")
  )

print(p5)
ggsave("diet_by_region.png", p5, width = 14, height = 10, dpi = 300)


# ============================================================
# 13. export
# ============================================================
write_csv(df_clean,       "boem_gut_content_cleaned.csv")
write_csv(foo,            "FOO_summary.csv")
write_csv(foo_id,         "FOO_identifiable_only.csv")
write_csv(weight_summary, "weight_summary.csv")

cat("\nDone! Output files written to working directory.\n")

# ============================================================
# 14. analyses
# ============================================================

# install if needed
install.packages("vegan")
library(vegan)

# Build a species matrix - one row per fish, one column per prey category
# values = FOO (1 if that prey was present in that stomach, 0 if not)
prey_matrix <- df_clean %>%
  filter(!is.na(prey_category_clean)) %>%
  mutate(present = 1) %>%
  distinct(fish_id, fish_species, region, prey_category_clean, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols     = c(fish_id, fish_species, region),
    names_from  = prey_category_clean,
    values_from = present,
    values_fill = 0
  )

# Pull out just the prey columns
prey_cols <- prey_matrix %>%
  select(-fish_id, -fish_species, -region)

# PERMANOVA: does diet differ by species?
adonis2(prey_cols ~ fish_species, data = prey_matrix, permutations = 999)

# PERMANOVA: does diet differ by region (East vs West)?
adonis2(prey_cols ~ region, data = prey_matrix %>% filter(!is.na(region)), 
        permutations = 999)

# Get NMDS scores and add species/region back
nmds <- metaMDS(prey_cols, distance = "bray", k = 2, trymax = 100)
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))

# Bind with the metadata columns from prey_matrix
nmds_df <- bind_cols(
  nmds_scores,
  prey_matrix %>% select(fish_species, region)
)

# Plot
ggplot(nmds_df, aes(x = NMDS1, y = NMDS2, color = fish_species)) +
  geom_point(size = 2, alpha = 0.7) +
  annotate("text", x = Inf, y = Inf, 
           label = paste("Stress =", round(nmds$stress, 3)),
           hjust = 1.1, vjust = 1.5, size = 3.5) +
  labs(title = "NMDS of Diet Composition by Species",
       color = "Species") +
  theme_minimal()