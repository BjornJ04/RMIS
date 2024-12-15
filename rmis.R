library(psych)

data <- readRDS(filename)
data$typing_tests_blocks <- as.numeric(data$typing_tests_blocks)
data$typing_tests_charTyped <- as.numeric(data$typing_tests_charTyped)
data$typing_tests_notifications_blocks <- as.numeric(data$typing_tests_notifications_blocks)
data$typing_tests_notifications_charTyped <- as.numeric(data$typing_tests_notifications_charTyped)
data$typing_tests_sound_blocks <- as.numeric(data$typing_tests_sound_blocks)
data$typing_tests_sound_charTyped <- as.numeric(data$typing_tests_sound_charTyped)

print('Shapiro Tests:')
print('Normal: Blocks:')
shapiro.test(data$typing_tests_blocks)

print('Normal: CharTyped:')
shapiro.test(data$typing_tests_charTyped)

print('Notification: Blocks:')
shapiro.test(data$typing_tests_notifications_blocks)

print('Notification: CharTyped:')
shapiro.test(data$typing_tests_notifications_charTyped)

print('Sound: Blocks:')
shapiro.test(data$typing_tests_sound_blocks)

print('Sound: CharTyped:')
shapiro.test(data$typing_tests_sound_charTyped)

print('Paired T-tests:')
t_test_normal_notification <- t.test(data$typing_tests_charTyped, data$typing_tests_notifications_charTyped, paired = TRUE)
print('Paired T-test normal vs notification charTyped (H1A):')
print(t_test_normal_notification)

t_test_normal_notification <- t.test(data$typing_tests_blocks, data$typing_tests_notifications_blocks, paired = TRUE)
print('Paired T-test normal vs notification blocks (H1B):')
print(t_test_normal_notification)

t_test_normal_notification <- t.test(data$typing_tests_charTyped, data$typing_tests_sound_charTyped, paired = TRUE)
print('Paired T-test normal vs sound charTyped (H1C):')
print(t_test_normal_notification)

t_test_normal_notification <- t.test(data$typing_tests_blocks, data$typing_tests_sound_blocks, paired = TRUE)
print('Paired T-test normal vs sound blocks (H1D):')
print(t_test_normal_notification)

t_test_normal_notification <- t.test(data$typing_tests_notifications_charTyped, data$typing_tests_sound_charTyped, paired = TRUE)
print('Paired T-test notification vs sound charTyped (H1C):')
print(t_test_normal_notification)

t_test_normal_notification <- t.test(data$typing_tests_notifications_blocks, data$typing_tests_sound_blocks, paired = TRUE)
print('Paired T-test notification vs sound blocks (H1D):')
print(t_test_normal_notification)

relevant_data <- data.frame(data$typing_tests_blocks, data$typing_tests_charTyped, data$typing_tests_notifications_blocks, data$typing_tests_notifications_charTyped, data$typing_tests_sound_blocks, data$typing_tests_sound_charTyped)
print(relevant_data)

filtered_data <- relevant_data[, !apply(relevant_data, 2, function(x) any(is.nan(x)))]

print(filtered_data)

print('Cronbachs alpha:')
alpha_result <- alpha(filtered_data, check.keys=TRUE)

print(alpha_result)
# SD H1A
differences <- data$typing_tests_charTyped - data$typing_tests_notifications_charTyped
sd_diff <- sd(differences)
print('SD H1A')
print(sd_diff)

# SD H1B
differences <- data$typing_tests_blocks - data$typing_tests_notifications_blocks
sd_diff <- sd(differences)
print('SD H1B')
print(sd_diff)

# SD H1C
differences <- data$typing_tests_charTyped - data$typing_tests_sound_charTyped
sd_diff <- sd(differences)
print('SD H1C')
print(sd_diff)

# SD H1D
differences <- data$typing_tests_blocks - data$typing_tests_sound_blocks
sd_diff <- sd(differences)
print('SD H1D')
print(sd_diff)

# SD H1E
differences <- data$typing_tests_notifications_charTyped - data$typing_tests_sound_charTyped
sd_diff <- sd(differences)
print('SD H1E notification vs sound charTyped')
print(sd_diff)

# SD H1F
differences <- data$typing_tests_notifications_blocks - data$typing_tests_sound_blocks
sd_diff <- sd(differences)
print('SD H1F notification vs sound blocks')
print(sd_diff)

boxplot(relevant_data)
normal_blocks <- relevant_data$data.typing_tests_blocks
notifications_blocks <- relevant_data$data.typing_tests_notifications_blocks
sound_blocks <- relevant_data$data.typing_tests_sound_blocks



boxplot(normal_blocks, notifications_blocks, sound_blocks,
        main = "Blocks per 60 seconds",
        names = c("Normal", "Notification", "Sound"),
        col = c("yellow","orange","red"),
        border = "brown",
        horizontal = FALSE,
        notch = FALSE
)

normal_charTyped <- relevant_data$data.typing_tests_charTyped
notifications_charTyped <- relevant_data$data.typing_tests_notifications_charTyped
sound_charTyped <- relevant_data$data.typing_tests_sound_charTyped

boxplot(normal_charTyped, notifications_charTyped, sound_charTyped,
        main = "Typed characters per 60 seconds",
        names = c("Normal", "Notification", "Sound"),
        col = c("yellow","orange","red"),
        border = "brown",
        horizontal = FALSE,
        notch = FALSE
)

