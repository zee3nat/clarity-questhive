# QuestHive Smart Contracts

QuestHive is a gamified lifestyle platform built on blockchain technology that helps users track habits, achieve goals, and engage with a supportive community. The platform features comprehensive achievement systems, social challenges, skill progression, and verified quest completion.

## Core Features

- Quest creation and tracking
- Achievement and rewards system
- Social interactions and challenges
- Skill tree progression
- Quest verification system
- Community engagement tools
- Custom leaderboards

## Smart Contracts

### Quest Contract (`quest.clar`)
Core contract for quest management with enhanced features:
- Quest creation with categories and tags
- Verification system for quest completion
- Custom categories and verification types
- Quest categorization and filtering

### Rewards Contract (`rewards.clar`)
Handles token distribution and achievement tracking:
- Token rewards for completing quests
- Streak multipliers for consistent completion
- Theme-based reward boosts
- Experience point tracking

### Profile Contract (`profile.clar`)
Manages user profiles and progression:
- Profile customization
- Experience and level tracking
- Badge management
- Reputation system
- Achievement tracking

### Social Contract (`social.clar`)
Enables community features and interactions:
- Friend connections
- Social challenges
- Group creation and management
- Shared quests
- Accomplishment sharing
- Leaderboards

### Skill Tree Contract (`skill-tree.clar`)
Implements progression and specialization system:
- Multiple specialization paths
- Skill unlocking with XP
- Prerequisites and progression tracking
- Custom skill paths (fitness, productivity, learning, etc.)
- Experience-based advancement

## Development

1. Install Clarinet
2. Run tests: `clarinet test`
3. Deploy contracts: `clarinet deploy`

## Contract Interactions

### Creating and Managing Quests
```clarity
;; Create a new quest
(contract-call? .quest create-quest "Daily Exercise" "30 minutes of cardio" u1 u86400 "fitness" (list "health" "cardio") true "image")

;; Complete a quest with verification
(contract-call? .quest submit-verification quest-id "verification-data")
```

### Social Features
```clarity
;; Create a social challenge
(contract-call? .social create-challenge recipient-address "Weekly Challenge" "Run 5k" "fitness" u100 u604800)

;; Create a group
(contract-call? .social create-group "Fitness Warriors" "Group for fitness enthusiasts")
```

### Skill Tree Progression
```clarity
;; Unlock a skill
(contract-call? .skill-tree unlock-skill skill-id)

;; Check skill prerequisites
(contract-call? .skill-tree can-unlock-skill? tx-sender skill-id)
```

### Rewards and Achievements
```clarity
;; Claim rewards with streak multiplier
(contract-call? .rewards reward-themed-completion tx-sender u1 u1)

;; Create achievement
(contract-call? .social create-accomplishment "First 5k" "Completed first 5k run" "fitness" u100 true)
```

## Security Considerations

- Quest verification requires proper authorization
- Social features include friend validation
- Skill tree progression requires prerequisite validation
- Reward multipliers have built-in caps
- Admin functions are protected

The platform implements various security checks and balances to ensure fair progression and prevent gaming of the reward system.

Note: This is a blockchain application - all transactions require STX for processing and are permanent once confirmed.