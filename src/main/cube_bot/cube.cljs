(ns cube-bot.cube)

(defonce combo
  ["Containment Priest",
   "Grand Abolisher",
   "Imposing Sovereign",
   "Kataki, War's Wage",
   "Leonin Arbiter",
   "Remorseful Cleric",
   "Thalia, Guardian of Thraben",
   "Tocatli Honor Guard",
   "Ethersworn Canonist",
   "Spirit of the Labyrinth",
   "Aven Mindcensor",
   "Loyal Retainers",
   "Recruiter of the Guard",
   "Sanctum Prelate",
   "Stonecloaker",
   "Auriok Salvagers",
   "Felidar Guardian",
   "Iona, Shield of Emeria",
   "Angel's Grace",
   "Enlightened Tutor",
   "Orim's Chant",
   "Silence",
   "Abeyance",
   "Abolish",
   "Balance",
   "Baral, Chief of Compliance",
   "Cloud of Faeries",
   "Snapcaster Mage",
   "Deceiver Exarch",
   "Laboratory Maniac",
   "Pestermite",
   "Trinket Mage",
   "Vendilion Clique",
   "Parasitic Strix",
   "Johnny, Combo Player",
   "Palinchron",
   "Tidespout Tyrant",
   "Tezzeret the Seeker",
   "Ancestral Recall",
   "Annul",
   "Brainstorm",
   "Chain of Vapor",
   "Dispel",
   "Envelop",
   "Flusterstorm",
   "High Tide",
   "Mental Misstep",
   "Mental Note",
   "Mystical Tutor",
   "Opt",
   "Peek",
   "Spell Pierce",
   "Spell Snare",
   "Steel Sabotage",
   "Swan Song",
   "Thought Scour",
   "Anticipate",
   "Arcane Denial",
   "Brain Freeze",
   "Censor",
   "Flash",
   "Framed!",
   "Impulse",
   "Into the Roil",
   "Mana Drain",
   "Muddle the Mixture",
   "Peer Through Depths",
   "Remand",
   "Telling Time",
   "Worldly Counsel",
   "Capsize",
   "Forbidden Alchemy",
   "Frantic Search",
   "Meditate",
   "Thirst for Knowledge",
   "Wipe Away",
   "Foil",
   "Gifts Ungiven",
   "Paradoxical Outcome",
   "Thwart",
   "Turnabout",
   "Force of Will",
   "Gush",
   "Misdirection",
   "Pact of Negation",
   "Repeal",
   "Commandeer",
   "Nexus of Fate",
   "Blue Sun's Zenith",
   "Dig Through Time",
   "Stroke of Genius",
   "Whir of Invention",
   "Careful Study",
   "Gitaxian Probe",
   "Ponder",
   "Preordain",
   "Serum Visions",
   "Sleight of Hand",
   "Chart a Course",
   "Ideas Unbound",
   "Merchant Scroll",
   "Shimmer of Possibility",
   "Strategic Planning",
   "Time Walk",
   "Transmute Artifact",
   "Compulsive Research",
   "Show and Tell",
   "Timetwister",
   "Tinker",
   "Windfall",
   "Mind's Desire",
   "Time Spiral",
   "Treasure Cruise",
   "Power Artifact",
   "Future Sight",
   "Mind Over Matter",
   "Nightscape Familiar",
   "Vampire Hexmage",
   "Maralen of the Mornsong",
   "Crypt Ghast",
   "Rune-Scarred Demon",
   "Griselbrand",
   "Dark Ritual",
   "Demonic Consultation",
   "Entomb",
   "Rain of Filth",
   "Scarab Feast",
   "Vampiric Tutor",
   "Cabal Ritual",
   "Plunge into Darkness",
   "Shred Memory",
   "Insidious Dreams",
   "Ad Nauseam",
   "Appetite for Brains",
   "Bubbling Muck",
   "Contract from Below",
   "Duress",
   "Imperial Seal",
   "Inquisition of Kozilek",
   "Reanimate",
   "Thoughtseize",
   "Collective Brutality",
   "Demonic Tutor",
   "Exhume",
   "Hymn to Tourach",
   "Night's Whisper",
   "Sign in Blood",
   "Cruel Bargain",
   "Doomsday",
   "Grim Tutor",
   "Infernal Contract",
   "Last Rites",
   "Painful Truths",
   "Praetor's Grasp",
   "Yawgmoth's Will",
   "Ill-Gotten Gains",
   "Tendrils of Agony",
   "Unmask",
   "Dark Petition",
   "Unburial Rites",
   "Mind Twist",
   "Animate Dead",
   "Dance of the Dead",
   "Waste Not",
   "Necro-Impotence",
   "Necromancy",
   "Necropotence",
   "Recurring Nightmare",
   "Yawgmoth's Bargain",
   "Goblin Welder",
   "Orcish Lumberjack",
   "Eidolon of the Great Revel",
   "Imperial Recruiter",
   "Scab-Clan Berserker",
   "Simian Spirit Guide",
   "Squee, Goblin Nabob",
   "Kiki-Jiki, Mirror Breaker",
   "Worldgorger Dragon",
   "Greater Gargadon",
   "Daretti, Scrap Savant",
   "Pyroblast",
   "Red Elemental Blast",
   "Desperate Ritual",
   "Pyretic Ritual",
   "Blast from the Past",
   "Mogg Salvage",
   "Seething Song",
   "Through the Breach",
   "Faithless Looting",
   "Gamble",
   "Rite of Flame",
   "Recoup",
   "Erratic Explosion",
   "Wheel of Fortune",
   "Empty the Warrens",
   "Past in Flames",
   "Reforge the Soul",
   "Devil's Play",
   "Bonfire of the Damned",
   "Impact Tremors",
   "Mana Flare",
   "Experimental Frenzy",
   "Sneak Attack",
   "Splinter Twin",
   "Stranglehold",
   "Magus of the Candelabra",
   "Tinder Wall",
   "Xantid Swarm",
   "Rofellos, Llanowar Emissary",
   "Azusa, Lost but Seeking",
   "Elvish Spirit Guide",
   "Eternal Witness",
   "Ramunap Excavator",
   "Oracle of Mul Daya",
   "Terastodon",
   "Woodfall Primus",
   "Worldspine Wurm",
   "Crop Rotation",
   "Nature's Claim",
   "Noxious Revival",
   "Worldly Tutor",
   "Early Harvest",
   "Ancient Stirrings",
   "Channel",
   "Explore",
   "Nostalgic Dreams",
   "Regrowth",
   "Summer Bloom",
   "Eureka",
   "Carpet of Flowers",
   "Exploration",
   "Fastbond",
   "Gaea's Touch",
   "Survival of the Fittest",
   "Sylvan Library",
   "Heartbeat of Spring",
   "Aluren",
   "Wilderness Reclamation",
   "Dictate of Karametra",
   "Cavern Harpy",
   "Dimir Charm",
   "Lim-D�l's Vault",
   "Glimpse the Unthinkable",
   "Notion Thief",
   "Whispering Madness",
   "Rakdos Charm",
   "Sire of Insanity",
   "Ruric Thar, the Unbowed",
   "Eladamri's Call",
   "Tidehollow Sculler",
   "Kambal, Consul of Allocation",
   "Sin Collector",
   "Treasured Find",
   "Squandered Resources",
   "Cadaverous Bloom",
   "Growth Spiral",
   "Bounding Krasis",
   "Tatyova, Benthic Druid",
   "Goblin Electromancer",
   "Izzet Charm",
   "Dack Fayden",
   "Saheeli Rai",
   "Electrolyze",
   "Invoke the Firemind",
   "Hellkite Overlord",
   "Naya Charm",
   "Bant Charm",
   "Wargate",
   "Dromar's Charm",
   "Mardu Charm",
   "Leovold, Emissary of Trest",
   "Sultai Charm",
   "Black Lotus",
   "Chrome Mox",
   "Jack-in-the-Mox",
   "Lion's Eye Diamond",
   "Lotus Bloom",
   "Lotus Petal",
   "Mana Crypt",
   "Mishra's Bauble",
   "Mox Diamond",
   "Mox Emerald",
   "Mox Jet",
   "Mox Opal",
   "Mox Pearl",
   "Mox Ruby",
   "Mox Sapphire",
   "Urza's Bauble",
   "Zuran Orb",
   "Altar of the Brood",
   "Candelabra of Tawnos",
   "Chromatic Sphere",
   "Chromatic Star",
   "Expedition Map",
   "Grindstone",
   "Mana Vault",
   "Sensei's Divining Top",
   "Sol Ring",
   "Voltaic Key",
   "Grim Monolith",
   "Helm of Awakening",
   "Isochron Scepter",
   "Pentad Prism",
   "Scroll Rack",
   "Time Vault",
   "Basalt Monolith",
   "Crucible of Worlds",
   "Horn of Greed",
   "Staff of Domination",
   "Temple Bell",
   "Umbral Mantle",
   "Thran Dynamo",
   "Memory Jar",
   "Mindslaver",
   "Mox Lotus",
   "Gleemax",
   "Hope of Ghirapur",
   "Lore Seeker",
   "Painter's Servant",
   "Phyrexian Revoker",
   "Metalworker",
   "Thought-Knot Seer",
   "Cogwork Librarian",
   "Lodestone Golem",
   "Myr Battlesphere",
   "Sundering Titan",
   "Void Winnower",
   "Walking Ballista",
   "Blightsteel Colossus",
   "Emrakul, the Aeons Torn",
   "Academy Ruins",
   "Ancient Tomb",
   "Bazaar of Baghdad",
   "City of Traitors",
   "Dark Depths",
   "Desolate Lighthouse",
   "Inventors' Fair",
   "Lake of the Dead",
   "Library of Alexandria",
   "Mishra's Workshop",
   "Rishadan Port",
   "Shelldock Isle",
   "Strip Mine",
   "Thespian's Stage",
   "Tolaria West",
   "Tolarian Academy",
   "Volrath's Stronghold",
   "Advantageous Proclamation",
   "Double Stroke",
   "Immediate Action",
   "Sentinel Dispatch",
   "Summoner's Bond",
   "Expansion // Explosion",
   "Discovery // Dispersal",
   "Bolas's Citadel",
   "Karn, the Great Creator",
   "Narset, Parter of Veils",
   "Manifold Key",
   "Veil of Summer",
   "Emry, Lurker of the Loch",
   "Escape to the Wilds",
   "Once Upon a Time",
   "Wishclaw Talisman",
   "Thassa's Oracle",
   "Underworld Breach",
   "Song of Creation",
   "Whirlwind of Thought",
   "Zirda, the Dawnwaker",
   "Jegantha, the Wellspring",
   "Freed from the Real",
   "Echo of Eons",
   "Force of Negation",
   "Llanowar Tribe",
   "Ransack the Lab",
   "Lutri, the Spellchaser"])
