/*
Note (santi):
- This class implements the game story script. All the events that constitute the main storyline are controlled from this class.
- Notice that all the sentences said by the NPCs are actually generated using natural language generation, rather than being
  hardcoded here. That is so that they can have an internal representation of them, and they can reason about them.
- Only the text said by the player character in thought bubbles is hardcoded directly as text.
- I should probably turn this class into some sort of XML or JSON file with the events properly defined, since it very quickly
  grew into a mess. But the flexibility of having it all here in code is hard to match :)
*/
var ShrdluGameScript = /** @class */ (function () {
    function ShrdluGameScript(game, app) {
        this.app = null;
        this.game = null;
        this.qwertyID = null;
        this.etaoinID = null;
        this.playerID = null;
        this.contextEtaoin = null;
        this.contextQwerty = null;
        this.contextShrdlu = null;
        this.contextPlayer = null;
        this.thoughtBubbleQueue = [];
        this.act = "intro";
        this.act_intro_state = 0;
        this.act_intro_state_timer = 0;
        this.act_intro_state_start_time = 0;
        this.act_intro_chair_x = -1;
        this.act_intro_chair_y = -1;
        this.act_1_state = 0;
        this.act_1_state_timer = 0;
        this.act_1_state_start_time = 0;
        this.act_1_number_of_useless_etaoin_answers = 0;
        this.act_1_explored_rooms = [];
        this.act_1_know_etaoin_is_an_AI = false;
        this.act_1_player_gone_outside = false;
        this.act_1_asked_about_being_alone_to_etaoin = false;
        this.act_1_asked_about_being_alone_to_qwerty = false;
        this.act_1_stasis_thread_state = 0;
        this.act_1_stasis_thread_state_timer = 0;
        this.act_1_asked_about_tools = false;
        this.act_1_asked_about_battery = false;
        this.act_1_stated_spacesuit_is_broken = false;
        this.act_1_asked_about_bruce_alper = [false, false, false];
        this.act_1_asked_about_corpse = [false, false, false];
        this.act_2_state = 0;
        this.act_2_state_timer = 0;
        this.act_2_state_start_time = 0;
        this.act_2_datapad_state = 0;
        this.act_2_datapad_state_timer = 0;
        this.act_2_repair_shuttle_state = 0;
        this.act_2_repair_shuttle_state_timer = 0;
        this.act_3_state = 0;
        this.act_3_state_timer = 0;
        this.act_3_state_start_time = 0;
        this.act_3_repair_tardis_console_state = 0;
        this.act_3_repair_tardis_console_state_timer = 0;
        this.act_3_repair_tardis_cable_state = 0;
        this.act_3_repair_tardis_cable_state_timer = 0;
        this.finding_life_side_plot_taken_question = false;
        this.finding_life_side_plot_analyzed_question = false;
        this.what_is_dust_side_plot_taken_question = false;
        this.player_has_asked_to_take_shrdlu = false;
        this.triggerOutOfCommRangeMessage = false;
        this.app = app;
        this.game = game;
        this.qwertyID = game.qwertyAI.selfID;
        this.etaoinID = game.etaoinAI.selfID;
        this.playerID = game.currentPlayer.ID;
    }
    ShrdluGameScript.prototype.update = function () {
        if (this.act == "intro") {
            // this.skip_to_act_end_of_intro();
            //this.skip_to_act_1();
            // this.skip_to_end_of_act_1();
            // this.skip_to_act_2();
            // this.skip_to_act_2_shrdluback();
            // this.skip_to_act_2_shrdluback_repair_outside();
            // this.skip_to_act_2_crash_site();
            // this.skip_to_act_2_after_crash_site();
            // this.skip_to_end_of_act_2();
            // this.skip_to_tardis8();
            // this.skip_to_tardis8_computer_room();
            // this.skip_to_act_3_back_from_tardis();
        }
        if (this.act == "intro")
            this.update_act_intro();
        if (this.act == "1")
            this.update_act_1();
        if (this.act == "2")
            this.update_act_2();
        if (this.act == "3")
            this.update_act_3();
        this.update_sideplots();
        this.processQueuedThoughtBubbles();
    };
    // These are debug functions, remove once the game is complete!
    ShrdluGameScript.prototype.skip_to_act_end_of_intro = function () {
        if (this.act_intro_state >= 101)
            return;
        this.game.currentPlayer.getOutOfBed(this.game);
        var term = Term.fromString("verb.wake-up('player'[#id])", this.game.ontology);
        this.game.qwertyAI.addLongTermTerm(term, PERCEPTION_PROVENANCE);
        this.game.etaoinAI.addLongTermTerm(term, PERCEPTION_PROVENANCE);
        // bedroom:
        this.game.currentPlayer.x = 560;
        this.game.currentPlayer.y = 200;
        this.game.currentPlayer.direction = A4_DIRECTION_DOWN;
        this.game.eyesClosedState = 2;
        this.game.textInputAllowed = true;
        this.game.qwertyAI.respondToPerformatives = true; // start responding to random questions from the player
        // start in the infirmary:
        this.game.currentPlayer.x = 12 * 8;
        this.game.currentPlayer.y = 28 * 8;
        this.act_intro_state = 101;
    };
    ShrdluGameScript.prototype.skip_to_act_1 = function () {
        this.game.currentPlayer.getOutOfBed(this.game);
        {
            var term_1 = Term.fromString("verb.wake-up('player'[#id])", this.game.ontology);
            this.game.qwertyAI.addLongTermTerm(term_1, PERCEPTION_PROVENANCE);
            this.game.etaoinAI.addLongTermTerm(term_1, PERCEPTION_PROVENANCE);
        }
        // bedroom:
        this.game.currentPlayer.x = 560;
        this.game.currentPlayer.y = 200;
        this.game.currentPlayer.direction = A4_DIRECTION_DOWN;
        this.game.currentPlayer.inventory.push(this.game.qwertyAI.robot.inventory[6]); // bedroom key
        this.game.qwertyAI.robot.inventory.splice(6, 1);
        this.game.currentPlayer.inventory.push(this.game.qwertyAI.robot.inventory[5]); // garage key
        this.game.qwertyAI.robot.inventory.splice(5, 1);
        var idx = this.game.qwertyAI.objectsNotAllowedToGive.indexOf("garage-key");
        this.game.qwertyAI.objectsNotAllowedToGive.splice(idx, 1);
        this.game.shrdluAI.allowPlayerInto("location-garage", "GARAGE");
        this.game.qwertyAI.allowPlayerInto("location-garage", "GARAGE");
        this.game.etaoinAI.allowPlayerInto("location-garage", "GARAGE");
        this.game.shrdluAI.allowPlayerInto("location-maintenance", "MAINTENANCE");
        this.game.qwertyAI.allowPlayerInto("location-maintenance", "MAINTENANCE");
        this.game.etaoinAI.allowPlayerInto("location-maintenance", "MAINTENANCE");
        this.game.currentPlayer.inventory.push(this.game.qwertyAI.robot.inventory[0]);
        this.game.qwertyAI.robot.inventory.splice(0, 1);
        this.game.eyesClosedState = 2;
        this.game.textInputAllowed = true;
        this.game.qwertyAI.respondToPerformatives = true; // start responding to random questions from the player
        this.game.etaoinAI.respondToPerformatives = true; // start responding to random questions from the player
        this.game.shrdluAI.respondToPerformatives = true; // start responding to random questions from the player
        this.act = "1";
        this.setupQwertyAgenda();
        // start in the infirmary:
        this.game.currentPlayer.x = 12 * 8;
        this.game.currentPlayer.y = 28 * 8;
        // start in the garage:
        //this.game.currentPlayer.x = 864;
        //this.game.currentPlayer.y = 40;
        //		this.act_1_state = 11;	// waiting for player to ask about other humans
        //		this.act_1_state = 15;	// etaoin will ask to go find Shrdlu
        this.act_1_state = 19;
        var term_h = Term.fromString("verb.need(E:'etaoin'[#id], verb.help(D:'player'[#id], E, verb.find(D, 'shrdlu'[#id])))", this.game.ontology);
        this.game.etaoinAI.addLongTermTerm(term_h, MEMORIZE_PROVENANCE);
        var term = Term.fromString("goal(D:'player'[#id], verb.find(X, 'shrdlu'[#id]))", this.game.ontology);
        this.game.etaoinAI.addLongTermTerm(term, MEMORIZE_PROVENANCE);
        this.game.currentPlayer.inventory.push(this.game.qwertyAI.robot.inventory[1]); // maintenance key
        this.game.qwertyAI.robot.inventory.splice(1, 1);
        this.game.currentPlayer.inventory.push(this.game.objectFactory.createObject("workingspacesuit", this.game, false, false));
        this.game.currentPlayer.inventory.push(this.game.objectFactory.createObject("full-battery", this.game, false, false));
        this.game.setStoryStateVariable("act1-corpse", "discovered");
        var term1 = Term.fromString("verb.happen('etaoin'[#id], erased('etaoin-memory'[#id]))", this.game.ontology);
        this.game.etaoinAI.addLongTermTerm(term1, PERCEPTION_PROVENANCE);
        this.game.qwertyAI.addLongTermTerm(term1, PERCEPTION_PROVENANCE);
        var term2 = Term.fromString("property.problem('etaoin'[#id], erased('etaoin-memory'[#id]))", this.game.ontology);
        this.game.etaoinAI.addLongTermTerm(term2, PERCEPTION_PROVENANCE);
        this.game.qwertyAI.addLongTermTerm(term2, PERCEPTION_PROVENANCE);
        var term3 = Term.fromString("property.strange(erased('etaoin-memory'[#id]))", this.game.ontology);
        this.game.etaoinAI.addLongTermTerm(term3, PERCEPTION_PROVENANCE);
        this.game.qwertyAI.addLongTermTerm(term3, PERCEPTION_PROVENANCE);
        // start in spacer valley
        //this.game.requestWarp(this.game.currentPlayer, this.game.maps[2], 40*8, 51*8);
        var suit_l = this.game.findObjectByID("spacesuit");
        (suit_l[0]).content.splice((suit_l[0]).content.indexOf(suit_l[1]), 1);
        this.game.currentPlayer.inventory.push(this.game.objectFactory.createObject("helmet", this.game, false, false));
    };
    ShrdluGameScript.prototype.skip_to_end_of_act_1 = function () {
        this.skip_to_act_1();
        // garage
        this.game.currentPlayer.x = 864;
        this.game.currentPlayer.y = 40;
        //this.game.qwertyAI.robot.x = 832;
        //this.game.qwertyAI.robot.y = 40;
        // infirmary
        //this.game.currentPlayer.x = 12*8;
        //this.game.currentPlayer.y = 28*8;
        this.game.setStoryStateVariable("rover", "working");
        //this.game.currentPlayer.inventory.push(this.game.objectFactory.createObject("luminiscent-dust", this.game, false, false));
        //this.game.setStoryStateVariable("luminiscent-fungi", "taken");
    };
    ShrdluGameScript.prototype.skip_to_act_2 = function () {
        this.skip_to_end_of_act_1();
        this.act = "2";
        // West cave:
        // this.game.currentPlayer.warp(4*8, 16*8, this.game.maps[5]);
        // Science lab:
        // this.game.currentPlayer.warp(13*8, 42*8, this.game.maps[0]);
        // room 6:
        // this.game.currentPlayer.warp(608, 216, this.game.maps[0]);
        // Infirmary:
        // this.game.currentPlayer.warp(12*8, 28*8, this.game.maps[0]);
        // East cave:
        //this.game.currentPlayer.warp(8*8, 12*8, this.game.maps[4]);
        this.contextShrdlu = this.game.shrdluAI.contextForSpeaker(this.playerID);
        this.game.setStoryStateVariable("act", "act2");
    };
    ShrdluGameScript.prototype.skip_to_act_2_shrdluback = function () {
        this.skip_to_act_2();
        this.game.currentPlayer.warp(864, 40, this.game.maps[0]);
        this.game.shrdluAI.robot.warp(864, 48, this.game.maps[0]);
        this.act_2_state = 106;
    };
    ShrdluGameScript.prototype.skip_to_act_2_shrdluback_repair_outside = function () {
        this.skip_to_act_2_shrdluback();
        // remove Shrdlu's goal:
        var s = Sentence.fromString("verb.search('shrdlu'[#id],[mineral])", this.game.ontology);
        this.game.shrdluAI.removeLongTermRule(s);
        var term = Term.fromString("goal(D:'player'[#id], verb.wait-for(X, 'shrdlu'[#id]))", this.game.ontology);
        this.game.etaoinAI.addLongTermTerm(term, MEMORIZE_PROVENANCE);
        this.act_2_state = 111;
        this.setupShrdluRepairCommsGoals();
        this.game.shrdluAI.visionActive = true;
        this.game.shrdluAI.robot.strength = 8;
    };
    ShrdluGameScript.prototype.skip_to_act_2_crash_site = function () {
        this.skip_to_act_2_shrdluback_repair_outside();
        this.game.etaoinAI.loadLongTermRulesFromFile("data/additional-kb-memoryrepair.xml");
        this.game.qwertyAI.loadLongTermRulesFromFile("data/additional-kb-memoryrepair.xml");
        this.game.shrdluAI.loadLongTermRulesFromFile("data/additional-kb-memoryrepair.xml");
        this.updateKnowledgeAfterRepairingCommTower();
        this.act_2_state = 222;
        this.setupShrdluAgendaGoals();
        this.game.currentPlayer.warp(40 * 8, 12 * 8, this.game.maps[6]); // crash site
        this.game.shrdluAI.robot.warp(32 * 8, 12 * 8, this.game.maps[6]); // crash site
        this.game.setStoryStateVariable("permission-to-take-shrdlu", "true");
    };
    ShrdluGameScript.prototype.skip_to_act_2_after_crash_site = function () {
        this.skip_to_act_2_crash_site();
        this.game.currentPlayer.x = 864;
        this.game.currentPlayer.warp(864, 40, this.game.maps[0]); // garage
        this.game.shrdluAI.robot.warp(864 - 16, 40, this.game.maps[0]); // garage
        this.act_2_state = 223;
        var item = this.game.objectFactory.createObject("shuttle-datapad", this.game, false, false);
        item.ID = "shuttle-datapad";
        this.game.currentPlayer.inventory.push(item);
        var engine = this.game.objectFactory.createObject("shuttle-engine", this.game, false, false);
        engine.ID = "shuttle-engine";
        this.game.shrdluAI.robot.inventory.push(engine);
    };
    ShrdluGameScript.prototype.skip_to_end_of_act_2 = function () {
        this.skip_to_act_2_crash_site();
        this.game.currentPlayer.warp(864, 40, this.game.maps[0]); // garage
        this.game.shrdluAI.robot.warp(864 - 16, 40, this.game.maps[0]); // garage
        this.act_2_state = 223;
        var item = this.game.objectFactory.createObject("fixed-datapad", this.game, false, false);
        item.ID = "shuttle-datapad";
        this.game.currentPlayer.inventory.push(item);
        // replace the background knowledge:
        for (var _i = 0, _a = [this.game.etaoinAI, this.game.qwertyAI, this.game.shrdluAI]; _i < _a.length; _i++) {
            var ai = _a[_i];
            var se = ai.longTermMemory.findSentenceEntry(Sentence.fromString("shuttle-datapad('shuttle-datapad'[#id])", this.game.ontology));
            if (se != null)
                se.sentence.terms[0].functor = this.game.ontology.getSort("fixed-datapad");
        }
        this.act_2_datapad_state = 0;
        this.game.etaoinAI.loadLongTermRulesFromFile("data/additional-kb-datapad.xml");
        this.game.qwertyAI.loadLongTermRulesFromFile("data/additional-kb-datapad.xml");
        this.game.shrdluAI.loadLongTermRulesFromFile("data/additional-kb-datapad.xml");
        this.game.setStoryStateVariable("act2-datapad", "read");
        // swap the shuttle:
        var shuttle = this.game.findObjectByIDJustObject("garage-shuttle");
        shuttle.map.removeObject(shuttle);
        this.game.requestDeletion(shuttle);
        var newShuttle = this.game.objectFactory.createObject("garage-shuttle", this.game, true, false);
        newShuttle.ID = shuttle.ID;
        newShuttle.direction = 2;
        var map = this.game.getMap("Aurora Station");
        newShuttle.warp(shuttle.x, shuttle.y, map);
        this.act_2_repair_shuttle_state = 2;
    };
    ShrdluGameScript.prototype.skip_to_tardis8 = function () {
        this.skip_to_end_of_act_2();
        this.game.loadTardis8LocationKnowledge();
        this.updateKnowledgeAfterReachingTrantorCrater();
        this.act = "3";
        this.act_3_state = 0;
        this.game.currentPlayer.inventory.push(this.game.objectFactory.createObject("extension-cord", this.game, false, false));
        //this.game.currentPlayer.warp(35*8, 29*8, this.game.maps[7]);	// trantor crater
        //this.game.shrdluAI.robot.warp(37*8, 29*8, this.game.maps[7]);	// trantor crater
        var shuttle = this.game.findObjectByIDJustObject("garage-shuttle");
        this.game.shrdluAI.robot.embark(shuttle);
        this.game.takeShuttleToTrantorCrater(shuttle, this.game.currentPlayer);
    };
    ShrdluGameScript.prototype.skip_to_tardis8_computer_room = function () {
        this.skip_to_tardis8();
        // turn the lights on:
        this.game.setStoryStateVariable("tardis-8-lights", "on");
        this.game.turnLightOn("tardis8-corridor-east");
        this.game.turnLightOn("tardis8-stasis1");
        this.game.turnLightOn("tardis8-stasis2");
        this.game.turnLightOn("tardis8-engineering");
        this.game.findObjectByIDJustObject("tardis-east-door-1").canBeOpen = true;
        this.game.findObjectByIDJustObject("tardis-east-door-2").canBeOpen = true;
        this.game.findObjectByIDJustObject("tardis-east-door-3").canBeOpen = true;
        this.game.findObjectByIDJustObject("tardis-east-door-4").canBeOpen = true;
        var thingToRepairObject = this.game.findObjectByIDJustObject("tardis-broken-cable");
        thingToRepairObject.map.removeObject(thingToRepairObject);
        this.game.requestDeletion(thingToRepairObject);
        // change the graphic of the cable:
        var map = this.game.getMap("Tardis 8");
        map.layers[0].tiles[44 + 16 * map.layers[0].width] = this.game.mapTiles[661];
        map.layers[1].tiles[43 + 16 * map.layers[0].width] = this.game.mapTiles[629];
        map.layers[0].cacheDrawTiles();
        map.layers[1].cacheDrawTiles();
        this.game.setStoryStateVariable("tardis-8-lights-west", "on");
        this.game.turnLightOn("tardis8-corridor-west");
        this.game.turnLightOn("tardis8-bridge");
        this.game.turnLightOn("tardis8-computer");
        this.game.findObjectByIDJustObject("tardis-west-door-1").canBeOpen = true;
        this.game.currentPlayer.disembark();
        this.game.currentPlayer.warp(21 * 8, 17 * 8, this.game.maps[8]);
        this.act_3_state = 5;
    };
    ShrdluGameScript.prototype.skip_to_act_3_back_from_tardis = function () {
        this.skip_to_tardis8_computer_room();
        var shuttle = this.game.findObjectByIDJustObject("garage-shuttle");
        this.game.takeShuttleFromTrantorCrater(shuttle);
        this.act_3_state = 7;
        var memoryCore = this.game.objectFactory.createObject("tardis-memory-core", this.game, false, false);
        memoryCore.ID = "tardis-memory-core";
        this.game.currentPlayer.inventory.push(memoryCore);
        this.game.setStoryStateVariable("tardis-memory-core", "discovered");
        //this.game.currentPlayer.warp(12*8, 28*8, this.game.maps[0]);	// infirmary
        this.game.currentPlayer.warp(864, 40, this.game.maps[0]); // garage
        var dust1 = this.game.objectFactory.createObject("luminiscent-dust", this.game, false, false);
        this.game.currentPlayer.inventory.push(dust1);
    };
    ShrdluGameScript.prototype.update_act_intro = function () {
        var previous_state = this.act_intro_state;
        if (this.act_intro_state >= 1 && this.contextQwerty == null)
            this.contextQwerty = this.game.qwertyAI.contextForSpeaker(this.playerID);
        if (this.game.currentPlayer.findObjectByID("player-masterkey") == null) {
            var key = this.game.objectFactory.createObject("master-key", this.game, false, false);
            key.ID = "player-masterkey";
            this.game.currentPlayer.inventory.push(key);
        }
        switch (this.act_intro_state) {
            /* --------------------------------------------------------
                Tutorial part 1: talking to qwerty.
               -------------------------------------------------------- */
            case 0:
                if (this.act_intro_state_timer > 60) {
                    this.qwertyIntention("action.talk($QWERTY, perf.greet($PLAYER))");
                    this.act_intro_state = 1;
                    this.game.textInputAllowed = true;
                }
                break;
            case 1: // qwerty is waiting to see if the player says something:
                if (this.contextQwerty.lastPerformativeBy(this.playerID) != null)
                    this.act_intro_state = 3;
                if (this.act_intro_state_timer > 240 && this.game.currentPlayer.talkingBubbleDuration == 0)
                    this.act_intro_state = 2;
                break;
            case 2: // give tutorial hint on how to talk
                if (this.act_intro_state_timer == 0) {
                    this.qwertyIntention("action.talk($QWERTY, perf.q.predicate($PLAYER, #and(X:verb.hear($PLAYER,$QWERTY), time.present(X))))");
                }
                else if (!this.game.qwertyAI.robot.isTalking()) {
                    this.app.tutorialMessages.push([" Everything is pitch black, and you  ",
                        " don't remember who or where you are!",
                        " But you want to try to respond...   ",
                        "",
                        " To say something, type what you want",
                        " to say, and then press *ENTER*.       ",
                        " For example, try to say \"*yes*\".      ",
                        "",
                        "  [press *ESC* to close this message]  "]);
                    this.act_intro_state = 3;
                }
                break;
            case 3:
                if (this.contextQwerty.lastPerformativeBy(this.playerID) != null) {
                    this.act_intro_state = 4;
                    this.contextQwerty.popLastQuestion(); // get rid of the question in case this was not a valid answer
                }
                else if (this.act_intro_state_timer >= 1200)
                    this.act_intro_state = 3;
                break;
            case 4:
                if (this.act_intro_state_timer == 0) {
                    this.qwertyIntention("action.talk($QWERTY, perf.sentiment($PLAYER, 'good'[symbol]))");
                    this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, awake($PLAYER)))");
                    var term = Term.fromString("verb.wake-up('player'[#id])", this.game.ontology);
                    this.game.qwertyAI.addLongTermTerm(term, PERCEPTION_PROVENANCE);
                    this.game.etaoinAI.addLongTermTerm(term, PERCEPTION_PROVENANCE);
                }
                else if (this.act_intro_state_timer == 120) {
                    this.game.eyesClosedState = 1;
                    this.game.eyesClosedTimer = 0;
                }
                else if (this.act_intro_state_timer == 120 + EYESOPEN_SPEED) {
                    this.act_intro_state = 5;
                }
                break;
            case 5: // QWERTY goes to do an analysis in the computer, reports player is healthy, and goes back to the infirmary bed:
                if (this.act_intro_state_timer == 0) {
                    this.qwertyIntention("action.talk($QWERTY, perf.request.action($PLAYER, #not(verb.move($PLAYER))))");
                    this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, verb.need($QWERTY, verb.do($QWERTY,[analysis]))))");
                    this.game.qwertyAI.respondToPerformatives = true; // start responding to random questions from the player
                    this.game.shrdluAI.respondToPerformatives = true; // start responding to random questions from the player
                    // move to a computer:
                    this.game.qwertyAI.clearCurrentAction();
                    this.qwertyMoves(9 * 8, 27 * 8, this.game.qwertyAI.robot.map);
                }
                else if (this.act_intro_state_timer == 320) {
                    // main character though:
                    this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "What is this place? and what am I doing here?", A4_DIRECTION_NONE, this.game);
                }
                else if (this.act_intro_state_timer == 560) {
                    this.game.qwertyAI.clearCurrentAction();
                    this.qwertyMoves(9 * 8, 29 * 8, this.game.qwertyAI.robot.map);
                }
                else if (this.act_intro_state_timer == 640) {
                    this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, healthy($PLAYER)))");
                    var term = Term.fromString("healthy('player'[#id])", this.game.ontology);
                    this.game.qwertyAI.addLongTermTerm(term, PERCEPTION_PROVENANCE);
                    // go back to player:
                    this.game.qwertyAI.clearCurrentAction();
                    this.qwertyMoves(6 * 8, 29 * 8, this.game.qwertyAI.robot.map);
                }
                else if (this.act_intro_state_timer == 800)
                    this.act_intro_state = 6;
                break;
            case 6: // QWERTY asks the player if he remembers his name
                if (this.act_intro_state_timer == 0) {
                    this.qwertyIntention("action.talk($QWERTY, perf.q.predicate($PLAYER, X:verb.remember($PLAYER,#and(#query(Y), name($PLAYER,Y)))))");
                }
                else {
                    if (this.game.qwertyAI.isIdle()) {
                        // the question has been answered:
                        var lastPerformative = this.contextQwerty.lastPerformativeBy(this.playerID);
                        if (lastPerformative != null && lastPerformative.performative != null &&
                            lastPerformative.performative.functor.is_a(this.game.ontology.getSort("perf.inform.answer"))) {
                            var answer = lastPerformative.performative.attributes[1];
                            if (answer instanceof ConstantTermAttribute) {
                                if (answer.value == "no" ||
                                    answer.value == "unknown") {
                                    this.act_intro_state = 7;
                                }
                                else if (answer.value == "yes") {
                                    this.act_intro_state = 8;
                                }
                                else {
                                    console.error("update_act_intro, state 5: unexpected answer " + lastPerformative.performative);
                                }
                            }
                            else {
                                // this can only mean the player answered with his name
                                this.act_intro_state = 9;
                            }
                        }
                        else {
                            // this can only mean the player answered with a statement, from which QWERTY could infer his name
                            // or it could be that the answer was complex, and had a counter question (which makes the answer NOT be the "lastPerformativeBy")
                            this.act_intro_state = 9;
                        }
                    }
                }
                break;
            case 7: // player says she does not remember her name, QWERTY dismisses it and moves on
                this.qwertyIntention("action.talk($QWERTY, perf.ack.ok($PLAYER))");
                this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, #and(X:verb.remember($PLAYER), time.later(X))))");
                this.act_intro_state = 10;
                break;
            case 8: // player says she remembers her name, QWERTY asks about it
                if (this.act_intro_state_timer == 0) {
                    this.qwertyIntention("action.talk($QWERTY, perf.q.query($PLAYER, Y, name($PLAYER,Y)))");
                }
                else {
                    if (this.game.qwertyAI.isIdle()) {
                        // the question has been answered:
                        var lastPerformative = this.contextQwerty.lastPerformativeBy(this.playerID);
                        if (lastPerformative != null && lastPerformative.performative != null &&
                            lastPerformative.performative.functor.is_a(this.game.ontology.getSort("perf.inform.answer"))) {
                            var answer = lastPerformative.performative.attributes[1];
                            if (answer instanceof VariableTermAttribute) {
                                if (answer.sort.name == "unknown") {
                                    this.act_intro_state = 7;
                                }
                                else {
                                    console.error("update_act_intro, state 7: unexpected answer " + lastPerformative.performative);
                                    this.act_intro_state = 7;
                                }
                            }
                            else {
                                // this can only mean the player answered with his name
                                this.act_intro_state = 9;
                            }
                        }
                        else {
                            // this can only mean the player answered with a statement, from which QWERTY could infer his name
                            this.act_intro_state = 9;
                        }
                    }
                }
                break;
            case 9: // player says her name, QWERTY and ETAOIN memorize it, and QWERTY acknowledges and greets again
                if (this.act_intro_state_timer == 0) {
                    this.qwertyIntention("action.talk($QWERTY, perf.sentiment($PLAYER, 'good'[symbol]))");
                    this.qwertyIntention("action.talk($QWERTY, perf.greet($PLAYER))");
                }
                else {
                    if (this.game.qwertyAI.isIdle())
                        this.act_intro_state = 10;
                }
                break;
            case 10: // QWERTY finally introduces himself
                if (this.act_intro_state_timer == 0) {
                    this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, name($QWERTY,'qwerty'[symbol])))");
                    this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, role($QWERTY, 'location-aurora-station'[#id], 'medic'[medic])))");
                    this.qwertyIntention("action.talk($QWERTY, perf.q.predicate($PLAYER,verb.remember($PLAYER,'location-aurora-station'[#id])))");
                }
                else {
                    if (this.game.qwertyAI.isIdle()) {
                        // the question has been answered:
                        var lastPerformative = this.contextQwerty.lastPerformativeBy(this.playerID);
                        if (lastPerformative != null && lastPerformative.performative != null &&
                            lastPerformative.performative.functor.is_a(this.game.ontology.getSort("perf.inform.answer"))) {
                            var answer = lastPerformative.performative.attributes[1];
                            if (answer instanceof ConstantTermAttribute) {
                                if (answer.value == "no" ||
                                    answer.value == "unknown") {
                                    this.act_intro_state = 12;
                                }
                                else if (answer.value == "yes") {
                                    this.act_intro_state = 11;
                                }
                                else {
                                    console.error("update_act_intro, state 9: unexpected answer " + lastPerformative.performative);
                                    this.act_intro_state = 12;
                                }
                            }
                            else {
                                console.error("update_act_intro, state 9: unexpected answer " + lastPerformative.performative);
                                this.act_intro_state = 12;
                            }
                        }
                        else {
                            console.error("update_act_intro, state 9: unexpected answer " + lastPerformative.performative);
                            this.act_intro_state = 12;
                        }
                    }
                }
                break;
            case 11: // player remembers the station, so QWERTY just says good, and moves on
                if (this.act_intro_state_timer == 0) {
                    this.qwertyIntention("action.talk($QWERTY, perf.sentiment($PLAYER, 'good'[symbol]))");
                }
                else {
                    if (this.game.qwertyAI.isIdle())
                        this.act_intro_state = 13;
                }
                break;
            case 12: // player does not remember the station, so QWERTY talks a bit, and moves on
                if (this.act_intro_state_timer == 0) {
                    this.qwertyIntention("action.talk($QWERTY, perf.sentiment($PLAYER, 'bad'[symbol]))");
                    this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER,#and(X:settlement('location-aurora-station'[#id])," +
                        "#and(time.permanent(X)," +
                        "#and(relation.purpose(X, #and(Y:[human], plural(Y))), " +
                        "space.at(X,'aurora'[#id]))))))");
                    this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, #and(X:verb.tell('etaoin'[#id], $PLAYER), time.later(X))))");
                }
                else {
                    if (this.game.qwertyAI.isIdle())
                        this.act_intro_state = 13;
                }
                break;
            case 13: // QWERTY asks the playter to stand up
                if (this.act_intro_state_timer == 0) {
                    this.qwertyIntention("action.talk($QWERTY, perf.ack.ok($PLAYER))");
                    this.qwertyIntention("action.talk($QWERTY, perf.request.action($PLAYER, verb.try($PLAYER,verb.stand($PLAYER))))");
                }
                else if (!this.game.qwertyAI.robot.isTalking()) {
                    this.act_intro_state = 14;
                }
                break;
            case 14: // player stands up
                if (this.act_intro_state_timer == 0) {
                    this.game.currentPlayer.getOutOfBed(this.game);
                    this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "Wow! I feel very light, why?! where am I?", A4_DIRECTION_NONE, this.game);
                }
                else if (this.act_intro_state_timer > 60 && !this.game.currentPlayer.isTalking()) {
                    this.qwertyIntention("action.talk($QWERTY, perf.sentiment($PLAYER, 'good'[symbol]))");
                    this.qwertyIntention("action.talk($QWERTY, perf.request.action($PLAYER, verb.try($PLAYER,verb.walk($PLAYER))))");
                    this.act_intro_state = 15;
                }
                break;
            case 15:
                if (!this.game.qwertyAI.robot.isTalking())
                    this.act_intro_state = 16;
                break;
            case 16:
                if (this.act_intro_state_timer == 0) {
                    this.app.tutorialMessages.push([" To move around, use the *ARROW KEYS*. ",
                        " For now, just walk around the room  ",
                        " and follow Qwerty's instructions.   ",
                        "",
                        " You can *hold SHIFT* to move faster.    ",
                        "",
                        "  [press *ESC* to close this message]  "]);
                }
                else {
                    if (this.game.currentPlayer.state == A4CHARACTER_STATE_WALKING)
                        this.act_intro_state = 98;
                }
                break;
            /* --------------------------------------------------------
                Tutorial part 2: walking around and interacting.
               -------------------------------------------------------- */
            case 98:
                if (this.act_intro_state_timer == 0) {
                    this.game.qwertyAI.clearCurrentAction();
                    this.qwertyMoves(11 * 8, 32 * 8, this.game.qwertyAI.robot.map); // move qwerty near the chair
                    this.qwertyIntention("action.talk($QWERTY, perf.sentiment($PLAYER,'good'[symbol]))");
                }
                else if (this.game.qwertyAI.robot.x == 11 * 8 &&
                    this.game.qwertyAI.robot.y == 32 * 8) {
                    this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, verb.want($QWERTY, verb.test($QWERTY, #and(C:[strength], verb.belong(C, $PLAYER))))))");
                    this.qwertyIntention("action.talk($QWERTY, perf.request.action($PLAYER, action.push($PLAYER, '512'[#id])))"); // 512 is the ID of the chair in the infirmary
                    this.game.qwertyAI.addLongTermTerm(Term.fromString("goal(D:'player'[#id],action.push(X, '512'[#id]))", this.game.ontology), MEMORIZE_PROVENANCE);
                    var chair_1 = this.game.findObjectByIDJustObject("512");
                    this.act_intro_chair_x = chair_1.x;
                    this.act_intro_chair_y = chair_1.y;
                    this.act_intro_state = 99;
                }
                break;
            case 99:
                if (!this.game.qwertyAI.robot.isTalking()) {
                    this.app.tutorialMessages.push([" To push or pull objects, walk up to ",
                        " them, *hold SPACE*, and then *press the",
                        " direction* in which you want to push ",
                        " them.                               ",
                        "",
                        " Walk to the chair in the south-east ",
                        " of the room and push it now.        ",
                        "",
                        "  [press *ESC* to close this message]  "]);
                    this.act_intro_state = 100;
                }
                break;
            case 100:
                var chair = this.game.findObjectByIDJustObject("512");
                if (chair.x != this.act_intro_chair_x || chair.y != this.act_intro_chair_y)
                    this.act_intro_state = 101;
                // give the player 15 seconds to figure it out, and otherwise, just show the tutorial message again
                if (this.act_intro_state_timer >= 1200)
                    this.act_intro_state = 99;
                break;
            case 101:
                if (this.act_intro_state_timer == 0) {
                    this.qwertyIntention("action.talk($QWERTY, perf.sentiment($PLAYER, 'good'[symbol]))");
                    this.qwertyIntention("action.talk($QWERTY, perf.request.action($PLAYER, verb.follow($PLAYER, $QWERTY)))");
                    var term = Term.fromString("goal(D:'player'[#id],verb.follow(X, 'qwerty'[#id]))", this.game.ontology);
                    this.game.qwertyAI.addLongTermTerm(term, MEMORIZE_PROVENANCE);
                }
                else if (this.act_intro_state_timer == 180) {
                    if (this.game.currentPlayer.isIdle()) {
                        this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "I don't know what this is all about, but let's just play along for now...", A4_DIRECTION_NONE, this.game);
                        this.game.qwertyAI.clearCurrentAction();
                        this.qwertyMoves(104, 232, this.game.qwertyAI.robot.map);
                    }
                    else {
                        this.act_intro_state_timer--;
                    }
                }
                else if (this.act_intro_state_timer > 180) {
                    if (!this.game.qwertyAI.robot.isTalking() &&
                        !this.game.currentPlayer.isTalking()) {
                        this.act_intro_state = 102;
                    }
                }
                break;
            case 102:
                if (this.game.qwertyAI.robot.x == 104 &&
                    this.game.qwertyAI.robot.y == 232) {
                    if (this.game.qwertyAI.robot.pixelDistance(this.game.currentPlayer) < 32) {
                        this.act_intro_state = 103;
                        this.game.qwertyAI.clearCurrentAction();
                        this.qwertyMoves(136, 232, this.game.qwertyAI.robot.map);
                    }
                }
                else if (this.act_intro_state_timer > 240 && this.game.qwertyAI.isIdle()) {
                    this.qwertyMoves(104, 232, this.game.qwertyAI.robot.map);
                    this.act_intro_state_timer = 0;
                }
                break;
            case 103:
                if (this.game.qwertyAI.robot.x == 136 &&
                    this.game.qwertyAI.robot.y == 232) {
                    if (this.game.qwertyAI.robot.pixelDistance(this.game.currentPlayer) < 32) {
                        this.act_intro_state = 104;
                        this.game.qwertyAI.clearCurrentAction();
                        this.qwertyMoves(136, 288, this.game.qwertyAI.robot.map);
                    }
                }
                else if (this.act_intro_state_timer > 240 && this.game.qwertyAI.isIdle()) {
                    this.qwertyMoves(136, 232, this.game.qwertyAI.robot.map);
                    this.act_intro_state_timer = 0;
                }
                break;
            case 104:
                if (this.game.qwertyAI.robot.x == 136 &&
                    this.game.qwertyAI.robot.y == 288) {
                    if (this.game.qwertyAI.robot.pixelDistance(this.game.currentPlayer) < 32) {
                        this.act_intro_state = 105;
                        this.game.qwertyAI.clearCurrentAction();
                        this.qwertyMoves(256, 288, this.game.qwertyAI.robot.map);
                    }
                }
                else if (this.act_intro_state_timer > 240 && this.game.qwertyAI.isIdle()) {
                    this.qwertyMoves(136, 288, this.game.qwertyAI.robot.map);
                    this.act_intro_state_timer = 0;
                }
                break;
            case 105:
                if (this.game.qwertyAI.robot.x == 256 &&
                    this.game.qwertyAI.robot.y == 288) {
                    if (this.game.qwertyAI.robot.pixelDistance(this.game.currentPlayer) < 32) {
                        this.act_intro_state = 106;
                        this.game.qwertyAI.clearCurrentAction();
                        this.qwertyMoves(544, 288, this.game.qwertyAI.robot.map);
                    }
                }
                else if (this.act_intro_state_timer > 240 && this.game.qwertyAI.isIdle()) {
                    this.qwertyMoves(256, 288, this.game.qwertyAI.robot.map);
                    this.act_intro_state_timer = 0;
                }
                break;
            case 106:
                if (this.game.qwertyAI.robot.x == 544 &&
                    this.game.qwertyAI.robot.y == 288) {
                    if (this.game.qwertyAI.robot.pixelDistance(this.game.currentPlayer) < 32) {
                        this.act_intro_state = 107;
                    }
                }
                else if (this.act_intro_state_timer > 240 && this.game.qwertyAI.isIdle()) {
                    this.qwertyMoves(544, 288, this.game.qwertyAI.robot.map);
                    this.act_intro_state_timer = 0;
                }
                break;
            case 107:
                if (this.act_intro_state_timer == 0) {
                    this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, verb.belong('location-as8'[#id], $PLAYER)))");
                }
                else if (this.game.qwertyAI.robot.state == A4CHARACTER_STATE_IDLE) {
                    this.qwertyDropsObject("communicator");
                    this.qwertyMoves(552, 288, this.game.qwertyAI.robot.map);
                    this.act_intro_state = 108;
                }
                break;
            case 108:
                if (this.act_intro_state_timer == 0) {
                    this.qwertyIntention("action.talk($QWERTY, perf.request.action($PLAYER, action.take($PLAYER, 'communicator'[#id])))");
                    var term = Term.fromString("goal(D:'player'[#id],action.take(X, 'communicator'[#id]))", this.game.ontology);
                    this.game.qwertyAI.addLongTermTerm(term, MEMORIZE_PROVENANCE);
                }
                else if (!this.game.qwertyAI.robot.isTalking()) {
                    this.app.tutorialMessages.push([" To take objects, walk onto them and ",
                        " press *SPACE*. Access your inventory  ",
                        " by pressing the *TAB* key. Take the   ",
                        " communicator now.",
                        "",
                        "  [press *ESC* to close this message]  "]);
                    this.act_intro_state = 109;
                }
                break;
            case 109:
                for (var _i = 0, _a = this.game.currentPlayer.inventory; _i < _a.length; _i++) {
                    var item = _a[_i];
                    if (item.ID == "communicator")
                        this.act_intro_state = 110;
                }
                break;
            case 110:
                if (this.act_intro_state_timer == 0) {
                    this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, #and(X:verb.can($PLAYER, #and(Y:action.talk($PLAYER), relation.target(Y, $ETAOIN))), relation.tool(X, 'communicator'[#id]))))");
                    this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, #and(X:verb.need($PLAYER, verb.sleep($PLAYER)), #and(time.first(X), conjunction-contrast(X)))))");
                    this.qwertyIntention("action.talk($QWERTY, perf.request.action($PLAYER, #and(X:verb.go($PLAYER, verb.sleep($PLAYER)), time.now(X))))");
                    var term = Term.fromString("goal(D:'player'[#id],verb.go-to(X, 'verb.sleep'[verb.sleep]))", this.game.ontology);
                    this.game.qwertyAI.addLongTermTerm(term, MEMORIZE_PROVENANCE);
                }
                else if (!this.game.qwertyAI.robot.isTalking()) {
                    this.act_intro_state = 111;
                }
                break;
            case 111:
                if (this.act_intro_state_timer == 0) {
                    this.app.tutorialMessages.push([" Some doors need to be operated by   ",
                        " hand. Take the key card qwerty just ",
                        " dropped, walk to your bedroom door  ",
                        " and open it. Press *SPACE* when facing",
                        " an object to interact with it.      ",
                        "",
                        "  [press *ESC* to close this message]  "]);
                    this.qwertyDropsObject("bedroom5-key");
                    this.qwertyMoves(560, 288, this.game.qwertyAI.robot.map);
                }
                else {
                    if (this.game.currentPlayer.x >= 528 && this.game.currentPlayer.x < 576 &&
                        this.game.currentPlayer.y >= 208 && this.game.currentPlayer.y < 256) {
                        // player entered bedroom 5
                        this.act_intro_state = 112;
                    }
                }
                break;
            case 112:
                if (this.game.currentPlayer.x >= 528 && this.game.currentPlayer.x < 576 &&
                    this.game.currentPlayer.y >= 208 && this.game.currentPlayer.y < 256 &&
                    this.game.currentPlayer.state == A4CHARACTER_STATE_IN_BED) {
                    // player went to bed in bedroom 5
                    this.game.currentPlayer.state = A4CHARACTER_STATE_IN_BED_CANNOT_GETUP;
                    this.act_intro_state = 113;
                }
                break;
            case 113:
                if (this.act_intro_state_timer == 0) {
                    this.game.eyesClosedState = 3;
                    this.game.eyesClosedTimer = 0;
                    this.game.textInputAllowed = false;
                }
                else if (this.act_intro_state_timer >= EYESOPEN_SPEED + 60) {
                    // bring qwerty back to the infirmary
                    this.game.requestWarp(this.game.qwertyAI.robot, this.game.qwertyAI.robot.map, 64, 232);
                    // clear the user goal:
                    this.game.qwertyAI.removeLongTermTermMatchingWith(Term.fromString("goal('player'[#id],X)", this.game.ontology));
                    this.act_intro_state = 114;
                }
                break;
            case 114:
                if (this.act_intro_state_timer == 0) {
                    this.game.in_game_seconds += 3600 * 7; // sleeping for 7 hours
                    this.game.narrationMessages.push("Several hours later...");
                    this.game.currentPlayer.x = 560;
                    this.game.currentPlayer.y = 216;
                    this.game.currentPlayer.direction = A4_DIRECTION_DOWN;
                }
                else if (this.act_intro_state_timer == 120) {
                    this.game.narrationMessages = [];
                }
                else if (this.act_intro_state_timer == 180) {
                    this.game.eyesClosedState = 1;
                    this.game.eyesClosedTimer = 0;
                    this.game.textInputAllowed = false;
                }
                else if (this.act_intro_state_timer == 180 + EYESOPEN_SPEED) {
                    this.game.currentPlayer.state = A4CHARACTER_STATE_IN_BED;
                    this.game.textInputAllowed = true;
                    this.act = "1";
                    this.act_1_state = 0;
                    this.setupQwertyAgenda();
                }
                break;
        }
        if (this.act_intro_state >= 110 &&
            this.act_intro_state_timer > 0 &&
            this.act_intro_state_timer % (60 * 30) == 0) {
            if (this.game.currentPlayer.isIdle()) {
                this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "I actually do feel sleepy, Qwerty was right, I should go to sleep now...", A4_DIRECTION_NONE, this.game);
            }
        }
        if (previous_state == this.act_intro_state) {
            this.act_intro_state_timer++;
        }
        else {
            this.act_intro_state_timer = 0;
            this.act_intro_state_start_time = this.game.in_game_seconds;
        }
    };
    /* --------------------------------------------------------
        ACT 1: Alone?
       -------------------------------------------------------- */
    ShrdluGameScript.prototype.update_act_1 = function () {
        var previous_state = this.act_1_state;
        if (this.contextQwerty == null)
            this.contextQwerty = this.game.qwertyAI.contextForSpeaker(this.playerID);
        if (this.act_1_state >= 6 && this.contextEtaoin == null)
            this.contextEtaoin = this.game.etaoinAI.contextForSpeaker(this.playerID);
        // check to see if the knowledge state needs to advance based on what the AI or the player say:
        if (this.act_1_state >= 6) {
            if (!this.act_1_know_etaoin_is_an_AI) {
                var p1 = this.contextEtaoin.lastPerformativeBy(this.game.etaoinAI.selfID);
                var p2 = this.contextQwerty.lastPerformativeBy(this.game.qwertyAI.selfID);
                if (p1 != null && p1.performative != null &&
                    p1.timeStamp == this.game.in_game_seconds - 1) {
                    var pattern = Term.fromString("perf.inform.answer('player'[#id], ai('etaoin'[#id]))", this.game.ontology);
                    if (p1.performative.equals(pattern)) {
                        this.act_1_know_etaoin_is_an_AI = true;
                    }
                }
                if (p2 != null &&
                    p2.timeStamp == this.game.in_game_seconds - 1) {
                    var pattern = Term.fromString("perf.inform.answer('player'[#id], ai('etaoin'[#id]))", this.game.ontology);
                    if (p2.performative.equals(pattern)) {
                        this.act_1_know_etaoin_is_an_AI = true;
                    }
                }
            }
        }
        if (!this.act_1_asked_about_being_alone_to_etaoin && this.contextEtaoin != null) {
            var p1 = this.contextEtaoin.lastPerformativeBy(this.playerID);
            if (p1 != null && p1.performative != null &&
                p1.timeStamp == this.game.in_game_seconds - 1) {
                var perf = p1.performative;
                if (this.questionAboutBeingAlone(perf)) {
                    this.act_1_asked_about_being_alone_to_etaoin = true;
                }
            }
        }
        if (!this.act_1_asked_about_being_alone_to_qwerty && this.contextQwerty != null) {
            var p1 = this.contextQwerty.lastPerformativeBy(this.playerID);
            if (p1 != null && p1.performative != null &&
                p1.timeStamp == this.game.in_game_seconds - 1) {
                var perf = p1.performative;
                if (this.questionAboutBeingAlone(perf)) {
                    this.act_1_asked_about_being_alone_to_qwerty = true;
                }
            }
        }
        var act_1_asked_about_bruce_alper_to_anyone = false;
        var act_1_asked_about_corpse_to_anyone = false;
        for (var i = 0; i < 3; i++) {
            if (this.act_1_asked_about_bruce_alper[i])
                act_1_asked_about_bruce_alper_to_anyone = true;
            if (this.act_1_asked_about_corpse[i])
                act_1_asked_about_corpse_to_anyone = true;
        }
        for (var i = 0; i < 3; i++) {
            var name_1 = "Etaoin";
            var ctx_1 = this.contextEtaoin;
            if (i == 1) {
                ctx_1 = this.contextQwerty;
                name_1 = "Qwerty";
            }
            if (i == 2) {
                ctx_1 = this.contextShrdlu;
                name_1 = "Shrdlu";
            }
            if (ctx_1 != null) {
                var p1 = ctx_1.lastPerformativeBy(this.playerID);
                var p2 = ctx_1.lastPerformativeBy(ctx_1.ai.selfID);
                if (p2 != null && p2.performative != null &&
                    p2.timeStamp == this.game.in_game_seconds - 1 &&
                    p1 != null && p1.performative != null) {
                    if (!this.act_1_asked_about_bruce_alper[i]) {
                        var patternq = Term.fromString("perf.q.whois.name(X:[any], Y:[any], name(Y, 'bruce alper'[symbol]))", this.game.ontology);
                        var patterna = Term.fromString("perf.inform.answer('player'[#id], 'unknown'[symbol])", this.game.ontology);
                        var perfq = p1.performative;
                        var perfa = p2.performative;
                        if (perfq != null && perfa != null &&
                            patternq.subsumes(perfq, true, new Bindings()) &&
                            patterna.subsumes(perfa, true, new Bindings())) {
                            // asked about bruce!
                            this.act_1_asked_about_bruce_alper[i] = true;
                            if (act_1_asked_about_bruce_alper_to_anyone) {
                                this.queueThoughtBubble(name_1 + " doesn't know about Bruce Alper either...");
                            }
                            else {
                                this.queueThoughtBubble(name_1 + " doesn't know about Bruce Alper, how come?!");
                            }
                        }
                    }
                    if (!this.act_1_asked_about_corpse[i]) {
                        var patternq = Term.fromString("perf.q.whois.noname(X:[any], 'corpse'[#id])", this.game.ontology);
                        var patterna = Term.fromString("perf.inform.answer('player'[#id], 'unknown'[symbol])", this.game.ontology);
                        var perfq = p1.performative;
                        var perfa = p2.performative;
                        if (perfq != null && perfa != null &&
                            patternq.subsumes(perfq, true, new Bindings()) &&
                            patterna.subsumes(perfa, true, new Bindings())) {
                            // asked about bruce!
                            this.act_1_asked_about_corpse[i] = true;
                            if (act_1_asked_about_corpse_to_anyone) {
                                this.queueThoughtBubble(name_1 + " doesn't know about the corpse either...");
                            }
                            else {
                                this.queueThoughtBubble("Hmm... " + name_1 + " doesn't know about the corpse. How strange! Maybe finding Shrdlu will clarify things!");
                            }
                        }
                    }
                }
            }
        }
        switch (this.act_1_state) {
            case 0:
                if (this.act_1_state_timer == 0) {
                    this.game.etaoinAI.respondToPerformatives = true; // start responding to random questions from the player
                }
                else {
                    if (this.game.currentPlayer.state == A4CHARACTER_STATE_IDLE) {
                        var term = Term.fromString("verb.wake-up('player'[#id])", this.game.ontology);
                        this.game.qwertyAI.addLongTermTerm(term, PERCEPTION_PROVENANCE);
                        this.game.etaoinAI.addLongTermTerm(term, PERCEPTION_PROVENANCE);
                        this.act_1_state = 1;
                    }
                }
                break;
            case 1:
                if (this.act_1_state_timer == 30) {
                    if (this.game.currentPlayer.isIdle()) {
                        this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "I feel better now and I'm getting used to feeling lighter, but still don't know what is this place...", A4_DIRECTION_NONE, this.game);
                    }
                    else {
                        this.act_1_state_timer--; // wait until player is idle!
                    }
                }
                else if (this.act_1_state_timer > 30 && this.game.currentPlayer.isIdle()) {
                    this.act_1_state = 2;
                }
                break;
            case 2:
                if (this.act_1_state_timer == 0) {
                    if (this.game.currentPlayer.isIdle()) {
                        this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "The last thing I remember is getting into stasis...", A4_DIRECTION_NONE, this.game);
                    }
                    else {
                        this.act_1_state_timer--; // wait until player is idle!
                    }
                }
                else if (this.act_1_state_timer > 0 && this.game.currentPlayer.isIdle()) {
                    this.act_1_state = 3;
                }
                break;
            case 3:
                if (this.act_1_state_timer == 0) {
                    if (this.game.currentPlayer.isIdle()) {
                        this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "Maybe this Etaoin person can answer my questions...", A4_DIRECTION_NONE, this.game);
                    }
                    else {
                        this.act_1_state_timer--; // wait until player is idle!
                    }
                }
                else if (this.act_1_state_timer > 0 && this.game.currentPlayer.isIdle()) {
                    this.act_1_state = 4;
                }
                break;
            case 4:
                this.app.tutorialMessages.push([" If you have the communicator or are ",
                    " anywhere within Aurora Station, you ",
                    " can talk to Etaoin by first calling ",
                    " his attention. For example, you can ",
                    " say \"*Hi Etaoin!*\", and from then on, ",
                    " you can ask Etaoin questions.       ",
                    "",
                    "  [press *ESC* to close this message]  "]);
                this.act_1_state = 5;
                break;
            case 5:
                // waiting for player to start talking to ETAOIN:
                if (this.game.etaoinAI.contextForSpeakerWithoutCreatingANewOne(this.playerID) != null) {
                    this.act_1_state = 6;
                    this.app.achievement_complete_tutorial = true;
                    this.app.trigger_achievement_complete_alert();
                }
                if (this.act_1_state_timer >= 600)
                    this.act_1_state = 4;
                break;
            case 6:
                // counting useless answers by etaoin or player getting tired:
                var p1 = this.contextEtaoin.lastPerformativeBy(this.game.etaoinAI.selfID);
                var p2 = this.contextEtaoin.lastPerformativeBy(this.playerID);
                if (p1 != null && p1.performative != null) {
                    if (p1.timeStamp == this.game.in_game_seconds - 1) {
                        // it just happened:
                        if (p1.performative.functor.name == "perf.inform.parseerror")
                            this.act_1_number_of_useless_etaoin_answers++;
                        if (p1.performative.functor.name == "perf.inform.answer" &&
                            p1.performative.attributes[1] instanceof ConstantTermAttribute &&
                            p1.performative.attributes[1].value == 'unknown')
                            this.act_1_number_of_useless_etaoin_answers++;
                        if (this.act_1_number_of_useless_etaoin_answers >= 4)
                            this.act_1_state = 7;
                    }
                    else if (this.game.in_game_seconds - p1.timeStamp > 1200) {
                        this.act_1_state = 7;
                    }
                }
                if (p2 != null && p2.performative != null) {
                    if (p2.performative.functor.name == "perf.farewell") {
                        this.act_1_state = 7;
                    }
                }
                if (this.act_1_asked_about_being_alone_to_etaoin &&
                    this.game.etaoinAI.currentInferenceProcess == null && this.game.etaoinAI.queuedInferenceProcesses.length == 0)
                    this.act_1_state = 12;
                if (this.act_1_asked_about_being_alone_to_qwerty &&
                    this.game.etaoinAI.currentInferenceProcess == null && this.game.etaoinAI.queuedInferenceProcesses.length == 0 &&
                    this.game.qwertyAI.currentInferenceProcess == null && this.game.qwertyAI.queuedInferenceProcesses.length == 0)
                    this.act_1_state = 13;
                break;
            case 7:
                if (this.game.currentPlayer.isIdle()) {
                    this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "Etaoin doesn't seem very useful...", A4_DIRECTION_NONE, this.game);
                    this.act_1_state = 8;
                }
                break;
            case 8:
                if (this.game.currentPlayer.isIdle()) {
                    this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "Maybe I should try to see if there is someone else in the station...", A4_DIRECTION_NONE, this.game);
                    this.act_1_state = 9;
                    this.act_1_explored_rooms = [];
                }
                break;
            case 9:
                if (this.game.currentPlayer.isIdle()) {
                    var currentRoom = this.game.getAILocation(this.game.currentPlayer);
                    if (currentRoom != null) {
                        var name_2 = currentRoom.name;
                        if (name_2 == null)
                            name_2 = currentRoom.sort.name;
                        if (name_2.indexOf("corridor") == -1 &&
                            name_2 != "infirmary" &&
                            name_2 != "bedroom 5" &&
                            name_2 != "aurora station") {
                            if (this.act_1_explored_rooms.indexOf(name_2) == -1) {
                                var messages = ["There is no one here...",
                                    "No one here either...",
                                    "no one here, let's keep looking...",
                                    "Empty as well?! Ok, let's check one more room..."];
                                if (this.act_1_explored_rooms.length < 4) {
                                    this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, messages[this.act_1_explored_rooms.length], A4_DIRECTION_NONE, this.game);
                                    this.act_1_explored_rooms.push(name_2);
                                }
                                else {
                                    this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "Wait, is there actually any other human in this station? I should ask Etaoin!", A4_DIRECTION_NONE, this.game);
                                    this.act_1_state = 10;
                                }
                            }
                        }
                    }
                    if (this.act_1_asked_about_being_alone_to_etaoin &&
                        this.game.etaoinAI.currentInferenceProcess == null && this.game.etaoinAI.queuedInferenceProcesses.length == 0)
                        this.act_1_state = 12;
                    if (this.act_1_asked_about_being_alone_to_qwerty &&
                        this.game.etaoinAI.currentInferenceProcess == null && this.game.etaoinAI.queuedInferenceProcesses.length == 0 &&
                        this.game.qwertyAI.currentInferenceProcess == null && this.game.qwertyAI.queuedInferenceProcesses.length == 0)
                        this.act_1_state = 13;
                }
                break;
            case 10:
                if (this.game.currentPlayer.isIdle()) {
                    if (!this.act_1_know_etaoin_is_an_AI) {
                        this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "And who is this Etaoin anyway?", A4_DIRECTION_NONE, this.game);
                        this.act_1_state = 11;
                    }
                    else {
                        this.act_1_state = 11;
                    }
                }
                break;
            case 11:
                // waiting for player to ask about other humans (and for etaoin not to be answering anything):
                if (this.act_1_asked_about_being_alone_to_etaoin && this.game.etaoinAI.isIdle())
                    this.act_1_state = 12;
                if (this.act_1_asked_about_being_alone_to_qwerty && this.game.etaoinAI.isIdle() && this.game.qwertyAI.isIdle())
                    this.act_1_state = 13;
                if (this.act_1_state == 11 && this.act_1_state_timer == 3600) {
                    // after a while, remind the player to actually ask Etaoin about being alone:
                    this.act_1_state_timer = 0;
                    this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "I still think I should ask Etaoin whether there is any other human in this station...", A4_DIRECTION_NONE, this.game);
                }
                break;
            case 12:
                // question was asked to Etaoin
                // wait a bit
                if (this.act_1_state_timer == 300) {
                    if (!this.contextEtaoin.inConversation) {
                        this.etaoinSays("perf.callattention('player'[#id])");
                    }
                    this.etaoinSays("perf.inform(D:'player'[#id],#and(V:verb.run('etaoin'[#id], [analysis]), #and(relation.effect(V, #and(Q:[perf.question], #and(verb.own(D, Q), plural(Q)))), time.past(V))))");
                    this.etaoinSays("perf.inform('player'[#id],#and(V:verb.find(E:'etaoin'[#id], X:[anomaly]), #and(time.past(V), space.at(V, M:'etaoin-memory'[#id]))))");
                }
                else if (this.act_1_state_timer > 300) {
                    if (this.game.etaoinAI.isIdle())
                        this.act_1_state = 14;
                }
                break;
            case 13:
                // question was asked to qwerty
                // wait a bit
                if (this.act_1_state_timer == 300) {
                    if (!this.contextEtaoin.inConversation) {
                        this.etaoinSays("perf.callattention('player'[#id])");
                    }
                    this.etaoinSays("perf.inform(D:'player'[#id],#and(V:verb.run('etaoin'[#id], [analysis]), #and(relation.effect(V, #and(Q:[perf.question], #and(verb.own(D, Q), #and(relation.target(Q, 'qwerty'[#id]), plural(Q))))), time.past(V))))");
                    this.etaoinSays("perf.inform('player'[#id],#and(V:verb.find(E:'etaoin'[#id], X:[anomaly]), #and(time.past(V), space.at(V, M:'etaoin-memory'[#id]))))");
                }
                else if (this.act_1_state_timer > 300) {
                    if (this.game.etaoinAI.isIdle())
                        this.act_1_state = 14;
                }
                break;
            case 14:
                if (this.act_1_state_timer == 0) {
                    var term = Term.fromString("erased('etaoin-memory'[#id])", this.game.ontology);
                    this.game.etaoinAI.addLongTermTerm(term, PERCEPTION_PROVENANCE);
                    this.etaoinSays("perf.inform('player'[#id], #and(V:verb.find(E:'etaoin'[#id], #and(P1:erased(M:'etaoin-memory'[#id]), time.past(P1))), time.past(V)))");
                    this.etaoinSays("perf.inform('player'[#id], verb.need('etaoin'[#id], verb.look-at('shrdlu'[#id],'etaoin-memory'[#id])))");
                    var term1 = Term.fromString("verb.happen('etaoin'[#id], erased('etaoin-memory'[#id]))", this.game.ontology);
                    this.game.etaoinAI.addLongTermTerm(term1, PERCEPTION_PROVENANCE);
                    this.game.qwertyAI.addLongTermTerm(term1, PERCEPTION_PROVENANCE);
                    var term2 = Term.fromString("property.problem('etaoin'[#id], erased('etaoin-memory'[#id]))", this.game.ontology);
                    this.game.etaoinAI.addLongTermTerm(term2, PERCEPTION_PROVENANCE);
                    this.game.qwertyAI.addLongTermTerm(term2, PERCEPTION_PROVENANCE);
                    var term3 = Term.fromString("property.strange(erased('etaoin-memory'[#id]))", this.game.ontology);
                    this.game.etaoinAI.addLongTermTerm(term3, PERCEPTION_PROVENANCE);
                    this.game.qwertyAI.addLongTermTerm(term3, PERCEPTION_PROVENANCE);
                }
                else {
                    if (this.game.etaoinAI.isIdle())
                        this.act_1_state = 15;
                }
                break;
            case 15:
                if (this.act_1_state_timer == 0) {
                    if (!this.contextEtaoin.inConversation) {
                        this.etaoinSays("perf.callattention('player'[#id])");
                    }
                    var term_h = Term.fromString("verb.need(E:'etaoin'[#id], verb.help(D:'player'[#id], E, verb.find(D, 'shrdlu'[#id])))", this.game.ontology);
                    this.game.etaoinAI.addLongTermTerm(term_h, MEMORIZE_PROVENANCE);
                    this.etaoinSays("perf.q.action('player'[#id], verb.find('player'[#id], 'shrdlu'[#id]))");
                }
                else {
                    // waiting for an answer from the player to "would you please find shrdlu?"
                    var p = this.contextEtaoin.lastPerformativeBy(this.playerID);
                    if (p != null && p.performative != null) {
                        if (p.timeStamp == this.game.in_game_seconds - 1) {
                            if (p.performative.functor.is_a(this.game.ontology.getSort("perf.inform.answer")) &&
                                this.game.etaoinAI.isIdle()) {
                                var answer = p.performative.attributes[1];
                                if (answer instanceof ConstantTermAttribute) {
                                    if (answer.value == "no" ||
                                        answer.value == "unknown") {
                                        this.act_1_state = 16;
                                    }
                                    else if (answer.value == "yes") {
                                        this.act_1_state = 18;
                                    }
                                    else {
                                        console.error("update_act_1, state 14: unexpected answer " + p.performative);
                                    }
                                }
                                else {
                                    // this can only mean the player answered with his name
                                    this.act_1_state = 8;
                                }
                            }
                            else if (p.performative.functor.is_a(this.game.ontology.getSort("perf.ack.ok"))) {
                                this.act_1_state = 18;
                            }
                        }
                    }
                }
                break;
            case 16:
                // player said she will not want to go search for SHRDLU:
                if (this.act_1_state_timer == 0) {
                    this.etaoinSays("perf.ack.ok('player'[#id])");
                }
                else if (this.act_1_state_timer >= 60 * 60) { // after one minute, try again
                    this.act_1_state = 15;
                }
                else {
                    // we are waiting, and maybe the player will change her mind:
                    if (this.playerChangedHerMindAfterSayingNo("etaoin")) {
                        this.act_1_state = 15;
                    }
                }
                break;
            case 18:
                // player agreed to go search for SHRDLU:
                if (this.act_1_state_timer == 0) {
                    this.etaoinSays("perf.thankyou('player'[#id])");
                    this.etaoinSays("perf.inform('player'[#id], #and(V:verb.go-to(E:'shrdlu'[#id], 'location-east-cave'[#id]), #and(relation.purpose(V, verb.gather(E, #and(M:[mineral], plural(M)))), time.past(V))))");
                    this.etaoinSays("perf.inform('player'[#id], #and(#not(V:verb.come-back(E:'shrdlu'[#id])), time.past(V)))");
                    this.etaoinSays("perf.request.action('player'[#id], #and(V1:action.take('player'[#id], 'garage-rover'[#id]), relation.purpose(V1, verb.find('player'[#id],'shrdlu'[#id]))))");
                    this.etaoinSays("perf.inform('player'[#id], space.at('garage-rover'[#id], 'location-garage'[#id]))");
                    this.etaoinSays("perf.inform('player'[#id], verb.have('qwerty'[#id], 'garage-key'[#id]))");
                    // the player now has the goal to find shrdlue:
                    var term = Term.fromString("goal(D:'player'[#id],verb.find(X, 'shrdlu'[#id]))", this.game.ontology);
                    this.game.etaoinAI.addLongTermTerm(term, MEMORIZE_PROVENANCE);
                    // tell qwerty it can give the key to the player:
                    var idx = this.game.qwertyAI.objectsNotAllowedToGive.indexOf("garage-key");
                    this.game.qwertyAI.objectsNotAllowedToGive.splice(idx, 1);
                    this.game.shrdluAI.allowPlayerInto("location-garage", "GARAGE");
                    this.game.qwertyAI.allowPlayerInto("location-garage", "GARAGE");
                    this.game.etaoinAI.allowPlayerInto("location-garage", "GARAGE");
                    this.act_1_state = 19;
                }
                break;
            case 19:
                // detect when player asks about where are the tools:
                if (!this.act_1_asked_about_tools && this.contextEtaoin != null) {
                    var p1_1 = this.contextEtaoin.lastPerformativeBy(this.playerID);
                    if (p1_1 != null && p1_1.performative != null &&
                        p1_1.timeStamp == this.game.in_game_seconds - 1) {
                        var perf = p1_1.performative;
                        var v = null;
                        var toolsFound = false;
                        if (perf.functor.is_a(this.game.ontology.getSort("perf.q.whereis")) && perf.attributes.length == 2) {
                            var queryTerms = null;
                            v = perf.attributes[1];
                            if (v instanceof ConstantTermAttribute) {
                                queryTerms = [v];
                            }
                            else if (v instanceof TermTermAttribute) {
                                var query = v.term;
                                queryTerms = Term.elementsInList(query, "#and");
                            }
                            if (queryTerms != null) {
                                for (var _i = 0, queryTerms_1 = queryTerms; _i < queryTerms_1.length; _i++) {
                                    var ta = queryTerms_1[_i];
                                    if ((ta instanceof ConstantTermAttribute) &&
                                        ta.value == "wrench")
                                        toolsFound = true;
                                    if ((ta instanceof ConstantTermAttribute) &&
                                        ta.value == "screwdriver")
                                        toolsFound = true;
                                    if ((ta instanceof TermTermAttribute) &&
                                        ta.term.functor.name == "tool")
                                        toolsFound = true;
                                }
                            }
                        }
                        else if (perf.functor.is_a(this.game.ontology.getSort("perf.q.whereis")) && perf.attributes.length == 4) {
                            if ((perf.attributes[1] instanceof VariableTermAttribute) &&
                                (perf.attributes[3] instanceof TermTermAttribute)) {
                                var queryTerm = perf.attributes[3].term;
                                if (queryTerm.functor.is_a(this.game.ontology.getSort("tool")) &&
                                    queryTerm.attributes.length == 1) {
                                    toolsFound = true;
                                }
                            }
                        }
                        if (toolsFound) {
                            this.act_1_asked_about_tools = true;
                            this.etaoinSays("perf.inform('player'[#id], verb.have('qwerty'[#id], 'maintenance-key'[#id]))");
                            var idx = this.game.qwertyAI.objectsNotAllowedToGive.indexOf("maintenance-key");
                            this.game.qwertyAI.objectsNotAllowedToGive.splice(idx, 1);
                            this.game.shrdluAI.allowPlayerInto("location-maintenance", "MAINTENANCE");
                            this.game.qwertyAI.allowPlayerInto("location-maintenance", "MAINTENANCE");
                            this.game.etaoinAI.allowPlayerInto("location-maintenance", "MAINTENANCE");
                        }
                    }
                }
                if (!this.act_1_asked_about_battery && this.contextEtaoin != null) {
                    var p1_2 = this.contextEtaoin.lastPerformativeBy(this.playerID);
                    if (p1_2 != null && p1_2.performative != null &&
                        p1_2.timeStamp == this.game.in_game_seconds - 1) {
                        var perf = p1_2.performative;
                        if (perf.functor.is_a(this.game.ontology.getSort("perf.inform")) &&
                            perf.attributes.length > 1 &&
                            perf.attributes[1] instanceof TermTermAttribute) {
                            var argument = (perf.attributes[1]).term;
                            var pattern1 = Term.fromString("empty('empty-battery'[#id])", this.game.ontology);
                            var b = new Bindings();
                            if (pattern1.subsumes(argument, true, b)) {
                                this.act_1_asked_about_battery = true;
                                console.log("update_act_1, state 19: detected battery is empty 1");
                                this.etaoinSays("perf.inform('player'[#id], verb.can('player'[#id], #and(F:verb.fill('player'[#id], 'empty-battery'[#id]), space.at(F,'location-powerplant'[#id]) )))");
                            }
                        }
                        else if (perf.functor.is_a(this.game.ontology.getSort("perf.q.predicate")) &&
                            perf.attributes.length > 1 &&
                            perf.attributes[1] instanceof TermTermAttribute) {
                            var argument = (perf.attributes[1]).term;
                            var pattern1 = Term.fromString("battery(V)", this.game.ontology);
                            var pattern2 = Term.fromString("#and(full(V:[any]), V4:battery(V))", this.game.ontology);
                            var b = new Bindings();
                            if (pattern1.subsumes(argument, true, b) ||
                                pattern2.subsumes(argument, true, b)) {
                                this.act_1_asked_about_battery = true;
                                console.log("update_act_1, state 19: detected battery is empty 2");
                                this.etaoinSays("perf.inform('player'[#id], verb.can('player'[#id], #and(F:verb.fill('player'[#id], #and(BATTERY:[battery], plural(BATTERY))), space.at(F,'location-powerplant'[#id]) )))");
                            }
                        }
                        else if (perf.functor.is_a(this.game.ontology.getSort("perf.q.whereis")) &&
                            perf.attributes.length == 3 &&
                            (perf.attributes[1] instanceof VariableTermAttribute) &&
                            perf.attributes[2] instanceof TermTermAttribute) {
                            var argument = (perf.attributes[2]).term;
                            var pattern1 = Term.fromString("battery(V)", this.game.ontology);
                            var pattern2 = Term.fromString("#and(full(V:[any]), V4:battery(V))", this.game.ontology);
                            var b = new Bindings();
                            if (pattern1.subsumes(argument, true, b) ||
                                pattern2.subsumes(argument, true, b)) {
                                this.act_1_asked_about_battery = true;
                                console.log("update_act_1, state 19: detected battery is empty 3");
                                this.etaoinSays("perf.inform('player'[#id], verb.can('player'[#id], #and(F:verb.fill('player'[#id], #and(BATTERY:[battery], plural(BATTERY))), space.at(F,'location-powerplant'[#id]) )))");
                            }
                        }
                    }
                }
                // detect when player mentions the spacesuit is broken:
                if (!this.act_1_stated_spacesuit_is_broken && this.contextEtaoin != null) {
                    var p1_3 = this.contextEtaoin.lastPerformativeBy(this.playerID);
                    if (p1_3 != null && p1_3.performative != null &&
                        p1_3.timeStamp == this.game.in_game_seconds - 1) {
                        var perf = p1_3.performative;
                        if (perf.functor.is_a(this.game.ontology.getSort("perf.inform")) &&
                            perf.attributes.length > 1 &&
                            perf.attributes[1] instanceof TermTermAttribute) {
                            var argument = (perf.attributes[1]).term;
                            var pattern1 = Term.fromString("property.broken('spacesuit'[#id])", this.game.ontology);
                            var pattern2 = Term.fromString("#and(verb.have(V1:'spacesuit'[#id], T:[#id]), tear(T))", this.game.ontology);
                            var b = new Bindings();
                            if (pattern1.subsumes(argument, true, b) ||
                                pattern2.subsumes(argument, true, b)) {
                                this.act_1_stated_spacesuit_is_broken = true;
                                this.etaoinSays("perf.inform('player'[#id], verb.can('qwerty'[#id], verb.repair('qwerty'[#id], 'spacesuit'[#id])))");
                            }
                        }
                        else if ((perf.functor.is_a(this.game.ontology.getSort("perf.q.action")) ||
                            perf.functor.is_a(this.game.ontology.getSort("perf.request.action"))) &&
                            perf.attributes.length > 1 &&
                            perf.attributes[1] instanceof TermTermAttribute) {
                            var argument = (perf.attributes[1]).term;
                            var pattern1 = Term.fromString("verb.repair('etaoin'[#id],'spacesuit'[#id])", this.game.ontology);
                            var b = new Bindings();
                            if (pattern1.subsumes(argument, true, b)) {
                                this.act_1_stated_spacesuit_is_broken = true;
                                this.etaoinSays("perf.inform('player'[#id], verb.can('qwerty'[#id], verb.repair('qwerty'[#id], 'spacesuit'[#id])))");
                            }
                        }
                        else if (perf.functor.is_a(this.game.ontology.getSort("perf.q.whereis")) &&
                            perf.attributes.length == 3 &&
                            (perf.attributes[1] instanceof ConstantTermAttribute) &&
                            perf.attributes[2] instanceof TermTermAttribute) {
                            var argument = (perf.attributes[2]).term;
                            var pattern1 = Term.fromString("V2:#and(V3:verb.can(V1, V4:verb.repair(V1, V5:'hypothetical-object'[#id])), V6:spacesuit(V5))", this.game.ontology);
                            var pattern2 = Term.fromString("V2:verb.can(V1, V3:verb.repair(V1, V4:'spacesuit'[#id]))", this.game.ontology);
                            var b = new Bindings();
                            if (pattern1.subsumes(argument, true, b) ||
                                pattern2.subsumes(argument, true, b)) {
                                this.act_1_stated_spacesuit_is_broken = true;
                                this.etaoinSays("perf.inform('player'[#id], verb.can('qwerty'[#id], verb.repair('qwerty'[#id], 'spacesuit'[#id])))");
                            }
                        }
                    }
                }
                // end of act 1!
                if (this.game.currentPlayer.isIdle() &&
                    this.game.getStoryStateVariable("act") == "act2") {
                    this.game.introact_request = 2;
                    this.app.achievement_complete_act1 = true;
                    this.app.trigger_achievement_complete_alert();
                }
                break;
        }
        if (previous_state == this.act_1_state) {
            this.act_1_state_timer++;
        }
        else {
            this.act_1_state_timer = 0;
            this.act_1_state_start_time = this.game.in_game_seconds;
        }
        previous_state = this.act_1_stasis_thread_state;
        switch (this.act_1_stasis_thread_state) {
            case 0: // no progress in this thread yet
                // detect when the player has asked qwerty to fix the broken space suit:
                if (this.playerAsksQwertyToFixSpacesuit()) {
                    var target_l_1 = this.game.findObjectByID("spacesuit");
                    var weCanGetIt = false;
                    if (target_l_1 != null && target_l_1.length == 1 &&
                        this.game.qwertyAI.canSee("spacesuit"))
                        weCanGetIt = true;
                    if (target_l_1 != null && target_l_1.length == 2 &&
                        (target_l_1[0] == this.game.qwertyAI.robot ||
                            target_l_1[0] == this.game.currentPlayer))
                        weCanGetIt = true;
                    if (weCanGetIt) {
                        this.act_1_stasis_thread_state = 1;
                    }
                    else {
                        this.act_1_stasis_thread_state = 0;
                        this.game.qwertyAI.respondToPerformatives = true;
                        // I do not see the spacvesuit:
                        this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, #not(verb.see($QWERTY, 'spacesuit'[#id]))))");
                    }
                }
                break;
            // qwerty has been asked to repair the space suit
            case 1:
                if (this.act_1_stasis_thread_state_timer == 0) {
                    this.game.qwertyAI.respondToPerformatives = false; // to prevent the player messing up with the sequence
                    this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, #and(V:verb.repair($QWERTY, 'spacesuit'[#id]), time.future(V)) ))");
                    // clear whatever qwerty is doing now:
                    this.game.qwertyAI.clearCurrentAction();
                    var currentRoom = this.game.getAILocation(this.game.qwertyAI.robot);
                    if (currentRoom.id != "location-as25") {
                        // if we are not in the infirmary, qwerty asks the player to follow it:
                        this.qwertyIntention("action.talk($QWERTY, perf.request.action(V0:$PLAYER, verb.follow(V0, $QWERTY)))");
                    }
                }
                else {
                    if (this.game.qwertyAI.isIdle())
                        this.act_1_stasis_thread_state = 2;
                }
                break;
            // go towards david to take the suit:
            case 2:
                var target_l = this.game.findObjectByID("spacesuit");
                if (target_l == null) {
                    this.act_1_stasis_thread_state = 0;
                    this.game.qwertyAI.respondToPerformatives = true;
                }
                else {
                    var d_1 = this.game.qwertyAI.robot.pixelDistance(target_l[0]);
                    if (target_l.length == 1) {
                        // the spacesuit is in the floor:
                        if (d_1 > 0) {
                            this.game.qwertyAI.robot.AI.addPFTargetObject(A4CHARACTER_COMMAND_IDLE, 10, false, target_l[0], this.game);
                        }
                        else {
                            if (this.game.qwertyAI.robot.takeAction(this.game)) {
                                this.act_1_stasis_thread_state = 3;
                            }
                            else {
                                this.act_1_stasis_thread_state = 0;
                                this.game.qwertyAI.respondToPerformatives = true;
                            }
                        }
                    }
                    else {
                        // someone has the spacesuit (this could be qwerty itself):
                        if (d_1 > 16) {
                            this.game.qwertyAI.robot.AI.addPFTargetObject(A4CHARACTER_COMMAND_IDLE, 10, false, target_l[0], this.game);
                        }
                        else {
                            if (target_l[0] instanceof A4Character) {
                                // assume spacesuit is target_l[1]!!!
                                (target_l[0]).removeFromInventory(target_l[1]);
                                this.game.qwertyAI.robot.addObjectToInventory(target_l[1], this.game);
                                this.act_1_stasis_thread_state = 3;
                                this.game.playSound("data/sfx/itemPickup.wav");
                            }
                            else {
                                this.act_1_stasis_thread_state = 0;
                                this.game.qwertyAI.respondToPerformatives = true;
                            }
                        }
                    }
                }
                break;
            case 3:
                // qwerty has the spacesuit!:
                if (this.act_1_stasis_thread_state_timer == 0) {
                    this.qwertyMoves(5 * 8, 26 * 8, this.game.qwertyAI.robot.map);
                }
                else {
                    if (this.game.qwertyAI.robot.x == 5 * 8 &&
                        this.game.qwertyAI.robot.y == 26 * 8) {
                        // wait for the player:
                        var currentRoom = this.game.getAILocation(this.game.currentPlayer);
                        if (currentRoom.id == "location-as25") {
                            this.qwertyIntention("action.talk($QWERTY, perf.request.action(V0:$PLAYER, verb.wait(V0, [space.here])))");
                            this.act_1_stasis_thread_state = 4;
                        }
                    }
                }
                break;
            case 4:
                if (this.game.qwertyAI.isIdle()) {
                    // open the door:
                    this.game.qwertyAI.robot.issueCommandWithArguments(A4CHARACTER_COMMAND_INTERACT, -1, A4_DIRECTION_UP, null, this.game);
                    this.act_1_stasis_thread_state = 5;
                }
                break;
            case 5:
                // go to the shelves and wait!:
                if (this.act_1_stasis_thread_state_timer == 0) {
                    this.qwertyMoves(4 * 8, 17 * 8, this.game.qwertyAI.robot.map);
                }
                else {
                    if (this.game.qwertyAI.robot.x == 4 * 8 &&
                        this.game.qwertyAI.robot.y == 17 * 8) {
                        this.act_1_stasis_thread_state = 6;
                    }
                }
                break;
            case 6:
                // wait a bit
                if (this.act_1_stasis_thread_state_timer == 180)
                    this.act_1_stasis_thread_state = 7;
                break;
            case 7:
                // give the suit back to the player
                if (this.game.currentPlayer.pixelDistance(this.game.qwertyAI.robot) > 16) {
                    this.game.qwertyAI.robot.AI.addPFTargetObject(A4CHARACTER_COMMAND_IDLE, 10, false, this.game.currentPlayer, this.game);
                }
                else {
                    // give the space suit:
                    var idx = 0;
                    for (var i = 0; i < this.game.qwertyAI.robot.inventory.length; i++) {
                        if (this.game.qwertyAI.robot.inventory[i].ID == "spacesuit") {
                            idx = i;
                            break;
                        }
                    }
                    this.game.qwertyAI.robot.inventory.splice(idx, 1);
                    var fixedSuit = this.game.objectFactory.createObject("workingspacesuit", this.game, false, false);
                    fixedSuit.ID = "spacesuit";
                    this.game.currentPlayer.addObjectToInventory(fixedSuit, this.game);
                    // replace the background knowledge:
                    for (var _a = 0, _b = [this.game.etaoinAI, this.game.qwertyAI, this.game.shrdluAI]; _a < _b.length; _a++) {
                        var ai = _b[_a];
                        var se = ai.longTermMemory.findSentenceEntry(Sentence.fromString("brokenspacesuit('spacesuit'[#id])", this.game.ontology));
                        if (se != null)
                            se.sentence.terms[0].functor = this.game.ontology.getSort("workingspacesuit");
                    }
                    this.game.currentPlayer.map.addPerceptionBufferRecord(new PerceptionBufferRecord("give", this.game.qwertyAI.robot.ID, this.game.qwertyAI.robot.sort, this.game.currentPlayer.ID, this.game.currentPlayer.sort, null, fixedSuit.ID, fixedSuit.sort, this.game.qwertyAI.robot.x, this.game.qwertyAI.robot.y, this.game.qwertyAI.robot.x + this.game.qwertyAI.robot.getPixelWidth(), this.game.qwertyAI.robot.y + this.game.qwertyAI.robot.getPixelHeight()));
                    this.game.currentPlayer.eventWithObject(A4_EVENT_RECEIVE, this.game.qwertyAI.robot, fixedSuit, this.game.currentPlayer.map, this.game);
                    this.game.qwertyAI.robot.eventWithObject(A4_EVENT_ACTION_GIVE, this.game.currentPlayer, fixedSuit, this.game.currentPlayer.map, this.game);
                    this.game.playSound("data/sfx/itemPickup.wav");
                    this.game.inGameActionsForLog.push(["give(" + this.game.qwertyAI.selfID + "," + fixedSuit.ID + "," + this.game.currentPlayer.ID + ")", "" + this.game.in_game_seconds]);
                    this.game.qwertyAI.respondToPerformatives = true;
                    this.act_1_stasis_thread_state = 8;
                }
                break;
            case 8:
                // get out of the way
                if (this.game.qwertyAI.robot.x == 8 * 8 &&
                    this.game.qwertyAI.robot.y == 29 * 8) {
                    this.qwertyMoves(6 * 8, 29 * 8, this.game.qwertyAI.robot.map);
                }
                else {
                    this.qwertyMoves(8 * 8, 29 * 8, this.game.qwertyAI.robot.map);
                }
                this.act_1_stasis_thread_state = 9;
                break;
            case 9:
                if (this.game.currentPlayer.isIdle()) {
                    this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "I vaguely remember Qwerty taking me to the infirmary before I first woke up from that room he just went to... What is in there?", A4_DIRECTION_NONE, this.game);
                    this.act_1_stasis_thread_state = 10;
                }
                break;
            case 10:
                // waiting for player to go to stasis room:
                if (this.game.currentPlayer.isIdle()) {
                    var currentRoom = this.game.getAILocation(this.game.currentPlayer);
                    // "location-as26" is the stasis pod room
                    if (currentRoom != null && currentRoom.id == "location-as26") {
                        this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "This is a stasis chamber! I must have been in one of these pods... but who is in the closed one?!", A4_DIRECTION_NONE, this.game);
                        this.act_1_stasis_thread_state = 11;
                    }
                }
                if (this.playerAsksQwertyToFixSpacesuit()) {
                    this.qwertyIntention("action.talk($QWERTY, perf.inform(V0:$PLAYER, #not(property.broken('spacesuit'[#id]))))");
                }
                break;
            case 11:
                // waiting for player to interact with the pod with the corpse:
                var corpsePod = this.game.findObjectByIDJustObject("broken-stasis-pod");
                if (!corpsePod.closed) {
                    this.game.cutSceneActivated = CUTSCENE_CORPSE;
                    this.act_1_stasis_thread_state = 12;
                    this.game.setStoryStateVariable("act1-corpse", "discovered");
                }
                if (this.playerAsksQwertyToFixSpacesuit()) {
                    this.qwertyIntention("action.talk($QWERTY, perf.inform(V0:$PLAYER, #not(property.broken('spacesuit'[#id]))))");
                }
                break;
            case 12:
                if (this.playerAsksQwertyToFixSpacesuit()) {
                    this.qwertyIntention("action.talk($QWERTY, perf.inform(V0:$PLAYER, #not(property.broken('spacesuit'[#id]))))");
                }
                break;
        }
        if (previous_state == this.act_1_stasis_thread_state) {
            this.act_1_stasis_thread_state_timer++;
        }
        else {
            this.act_1_stasis_thread_state_timer = 0;
        }
        if (!this.act_1_player_gone_outside && this.game.currentPlayer.map.name == "Aurora Station Outdoors") {
            this.queueThoughtBubble("Oh wow! Look at this! So, I am indeed not on Earth!");
            this.queueThoughtBubble("Looking throught the windows cannot compare with being outside!");
            this.act_1_player_gone_outside = true;
        }
    };
    /* --------------------------------------------------------
        ACT 2: SHRDLU
       -------------------------------------------------------- */
    ShrdluGameScript.prototype.update_act_2 = function () {
        var previous_state = this.act_2_state;
        switch (this.act_2_state) {
            case 0:
                // wait a few cycles to ensure the character is already in the vehicle
                if (this.game.currentPlayer.isIdle() && this.act_2_state_timer >= 10) {
                    this.queueThoughtBubble("This rover seems bigger on the inside than it is on the outside!");
                    this.act_2_state = 1;
                }
                break;
            case 1:
                if (this.playerHasItemP("communicator")) {
                    if (this.thoughtBubbleQueue.length == 0 &&
                        this.game.currentPlayer.isIdle()) {
                        this.etaoinSays("perf.greet('player'[#id])");
                        this.etaoinSays("perf.inform(V0:'player'[#id], #and(V:verb.guide('etaoin'[#id], 'player'[#id], 'location-east-cave'[#id]), time.future(V)))");
                        this.etaoinSays("perf.inform(V0:'player'[#id], time.subsequently(verb.go(V0,'north'[north],'spacer-valley-north'[#id]), verb.go(V0,'east'[east],'location-east-cave'[#id])))");
                        this.etaoinSays("perf.inform(V0:'player'[#id], #and(V:space.outside.of('player'[#id], 'communicator-range'[#id]), time.future(V)))");
                        this.etaoinSays("perf.request.action(V0:'player'[#id], verb.find('player'[#id], 'shrdlu'[#id]))");
                        this.act_2_state = 2;
                    }
                }
                else {
                    // if the player does not have the communicator, but has made it to the north, just move the state up
                    if (this.game.currentPlayer.map.name == "Spacer Valley North")
                        this.act_2_state = 2;
                }
                break;
            case 2:
                if (this.playerHasItemP("communicator")) {
                    if (this.game.currentPlayer.isIdle() && this.game.currentPlayer.map.name == "Spacer Valley North") {
                        this.queueThoughtBubble("The communicator has gone dead. I guess I'm now out of communicator range with Etaoin...");
                        this.queueThoughtBubble("Etaoin said to go East after reaching this part of Spacer Valley, let's see where is that cave...");
                        this.act_2_state = 3;
                    }
                }
                else {
                    if (this.game.currentPlayer.map.name == "Spacer Valley North" &&
                        this.game.currentPlayer.x >= 64 * 8) {
                        this.act_2_state = 3;
                    }
                }
                break;
            case 3:
                if (this.game.currentPlayer.isIdle() && this.game.currentPlayer.map.name == "Spacer Valley North" &&
                    this.game.currentPlayer.x >= 64 * 8) {
                    this.queueThoughtBubble("I am picking up a distress signal that seems to come from that cave entrance to the east!");
                    this.queueThoughtBubble("That's probably Shrdlu, I should go investigate!");
                    this.act_2_state = 4;
                }
                break;
            case 4:
                if (this.game.currentPlayer.isIdle() && this.game.currentPlayer.map.name == "East Cave") {
                    this.queueThoughtBubble("It seems there was a cave-in here, maybe Shrdlu got trapped behind those rocks?");
                    this.act_2_state = 5;
                }
                break;
            case 5:
                // Player has found Shrdlu, but not started talking to it yet
                if (this.game.currentPlayer.isIdle() && this.game.currentPlayer.map.name == "Spacer Valley North") {
                    this.queueThoughtBubble("I did not see Shrdlu, but the signal came from this cave. Maybe if I talk Shrdlu can hear me?");
                    this.act_2_state = 6;
                }
                if (this.contextShrdlu == null) {
                    this.contextShrdlu = this.game.shrdluAI.contextForSpeaker(this.playerID);
                }
                else if (this.contextShrdlu.lastPerformativeBy("shrdlu") != null) {
                    this.act_2_state = 100;
                }
                break;
            case 6:
                // Player has found Shrdlu, but not started talking to it yet
                if (this.game.currentPlayer.isIdle() && this.game.currentPlayer.map.name == "Aurora Station") {
                    // First delete any knowledge Etaoin had on whether you have found SHRDLU or not (otherwise, you can create a contradiction):
                    var term_2 = Term.fromString("verb.find('player'[#id], 'shrdlu'[#id])", this.game.ontology);
                    var s_1 = new Sentence([term_2], [false]);
                    this.game.etaoinAI.removeLongTermRule(s_1);
                    this.etaoinSays("perf.greet('player'[#id])");
                    this.etaoinSays("perf.q.predicate(V0:'player'[#id], verb.find('player'[#id], 'shrdlu'[#id]))");
                    this.act_2_state = 7;
                }
                if (this.contextShrdlu == null) {
                    this.contextShrdlu = this.game.shrdluAI.contextForSpeaker(this.playerID);
                }
                else if (this.contextShrdlu.lastPerformativeBy("shrdlu") != null) {
                    this.act_2_state = 100;
                }
                break;
            case 7:
                // waiting for an answer from the player to "do you find shrdlu?"
                if (this.game.currentPlayer.map.name != "Aurora Station") {
                    this.act_2_state = 6;
                }
                else {
                    var p = this.contextEtaoin.lastPerformativeBy(this.playerID);
                    if (p != null && p.performative != null) {
                        if (p.timeStamp == this.game.in_game_seconds - 1) {
                            if (p.performative.functor.is_a(this.game.ontology.getSort("perf.inform.answer")) &&
                                this.game.etaoinAI.intentions.length == 0 &&
                                this.game.etaoinAI.queuedIntentions.length == 0 &&
                                this.contextEtaoin.expectingAnswerToQuestion_stack.length == 0) {
                                var answer = p.performative.attributes[1];
                                if (answer instanceof ConstantTermAttribute) {
                                    if (answer.value == "no" ||
                                        answer.value == "unknown") {
                                        this.act_2_state = 8;
                                    }
                                    else if (answer.value == "yes") {
                                        this.act_2_state = 10;
                                    }
                                    else {
                                        console.error("update_act_2, state 7: unexpected answer " + p.performative);
                                    }
                                }
                                else {
                                    // this can only mean the player answered with a name
                                    console.error("update_act_2, state 7: unexpected answer " + p.performative);
                                }
                            }
                            else if (p.performative.functor.is_a(this.game.ontology.getSort("perf.ack.ok"))) {
                                console.error("update_act_2, state 7: unexpected answer " + p.performative);
                            }
                        }
                    }
                }
                break;
            case 8:
                // player said "no" to having found Shrdlu
                if (this.game.currentPlayer.isIdle()) {
                    this.etaoinSays("perf.request.action(V0:'player'[#id], verb.find('player'[#id], 'shrdlu'[#id]))");
                    this.act_2_state = 9;
                }
                break;
            case 9:
                if (this.game.currentPlayer.map.name != "Aurora Station")
                    this.act_2_state = 6;
                break;
            case 10:
                // player said "yes" to having found Shrdlu
                if (this.game.currentPlayer.isIdle()) {
                    this.etaoinSays("perf.request.action(V0:'player'[#id], verb.bring('player'[#id], 'shrdlu'[#id], 'location-aurora-station'[#id]))");
                    this.act_2_state = 11;
                }
                break;
            case 11:
                // player has told Etaoin that he has found Shrdlu
                if (this.contextShrdlu == null) {
                    this.contextShrdlu = this.game.shrdluAI.contextForSpeaker(this.playerID);
                }
                else if (this.contextShrdlu.lastPerformativeBy("shrdlu") != null) {
                    this.act_2_state = 100;
                }
                break;
            case 100:
                // Conversation with Shrdlu has started!
                if (this.game.currentPlayer.map.name == "East Cave") {
                    var term_h = Term.fromString("verb.need(S:'shrdlu'[#id], verb.help(D:'player'[#id], S, verb.take-to(D, 'shrdlu'[#id], 'location-aurora-station'[#id])))", this.game.ontology);
                    this.game.shrdluAI.addLongTermTerm(term_h, MEMORIZE_PROVENANCE);
                    this.shrdluSays("perf.request.action(V0:'player'[#id], verb.help('player'[#id], 'shrdlu'[#id]))");
                    this.shrdluSays("perf.inform('player'[#id], #and(V:verb.damage('east-cave-cave-in'[#id], 'shrdlu-perception'[#id]), time.past(V)))");
                    this.shrdluSays("perf.inform(V0:'player'[#id], property.blind('shrdlu'[#id]))");
                    this.shrdluSays("perf.q.action('player'[#id], verb.help('player'[#id], 'shrdlu'[#id], verb.go-to('shrdlu'[#id], 'location-aurora-station'[#id])))");
                    this.act_2_state = 101;
                }
                break;
            case 101:
                if (this.game.shrdluAI.isIdle()) {
                    // the question has been answered:
                    var lastPerformative = this.contextShrdlu.lastPerformativeBy(this.playerID);
                    if (lastPerformative != null && lastPerformative.performative != null &&
                        lastPerformative.performative.functor.is_a(this.game.ontology.getSort("perf.inform.answer"))) {
                        var answer = lastPerformative.performative.attributes[1];
                        if (answer instanceof ConstantTermAttribute) {
                            if (answer.value == "no" ||
                                answer.value == "unknown") {
                                this.act_2_state = 102;
                            }
                            else if (answer.value == "yes") {
                                this.act_2_state = 103;
                            }
                            else {
                                console.error("update_act_2, state 101: unexpected answer " + lastPerformative.performative);
                                this.act_2_state = 102;
                            }
                        }
                        else {
                            this.act_2_state = 102;
                        }
                    }
                    else if (lastPerformative.performative.functor.is_a(this.game.ontology.getSort("perf.ack.ok"))) {
                        this.act_2_state = 103;
                    }
                    else {
                        this.act_2_state = 102;
                    }
                }
                break;
            case 102:
                if (this.act_2_state_timer >= 30 * 60) {
                    this.act_2_state = 100;
                }
                else {
                    // we are waiting, and maybe the player will change her mind:
                    if (this.playerChangedHerMindAfterSayingNo("shrdlu")) {
                        this.act_2_state = 100;
                    }
                }
                break;
            case 103:
                this.shrdluSays("perf.thankyou('player'[#id])");
                this.shrdluSays("perf.request.action(V0:'player'[#id], action.give('player'[#id], #and(V:[instruction], plural(V)), 'shrdlu'[#id]))");
                this.act_2_state = 104;
                break;
            case 104:
                // player is interacting with SHRDLU, as long as the player keeps doing things, we are fine, when she stops
                // asking shrdlu to do things, we give a clue
                if (this.game.shrdluAI.isIdle()) {
                    // the question has been answered:
                    var lastPerformative = this.contextShrdlu.lastPerformativeBy(this.playerID);
                    if (lastPerformative != null && lastPerformative.performative != null &&
                        lastPerformative.performative.functor.is_a_string("perf.request.action") ||
                        lastPerformative.performative.functor.is_a_string("perf.q.action")) {
                        this.act_2_state_timer = 0;
                    }
                }
                if (this.act_2_state_timer >= 30 * 60) {
                    // give a hint:
                    this.queueThoughtBubble("Shrdlu sounded from the northeast. If I can ask him to move in my direction, maybe he's strong enough to push these boulders...");
                    this.act_2_state = 105;
                }
                if (this.game.shrdluAI.robot.x < 128) {
                    // SHRDLU is free!
                    this.act_2_state = 106;
                }
                break;
            case 105:
                // same as 104, but we have already given the hint
                if (this.game.shrdluAI.robot.x < 128)
                    this.act_2_state = 106;
                break;
            case 106:
                // SHRDLU is free!!
                if (this.game.shrdluAI.robot.map.name == "Aurora Station") {
                    this.act_2_state = 107;
                }
                break;
            case 107:
                // SHRDLU is back!!
                var term = Term.fromString("goal(D:'player'[#id], verb.wait-for(X, 'shrdlu'[#id]))", this.game.ontology);
                this.game.etaoinAI.addLongTermTerm(term, MEMORIZE_PROVENANCE);
                // remove Shrdlu's goal:
                var s = Sentence.fromString("verb.search('shrdlu'[#id],[mineral])", this.game.ontology);
                this.game.shrdluAI.removeLongTermRule(s);
                this.contextShrdlu.inConversation = true; // this is a hack, to prevent it having to say "hello human", etc.
                this.shrdluSays("perf.thankyou('player'[#id])");
                this.shrdluSays("perf.inform('player'[#id], #and(X:verb.help('etaoin'[#id], 'shrdlu'[#id], verb.see('shrdlu'[#id])), time.now(X)))");
                this.shrdluSays("perf.inform('player'[#id], verb.need('shrdlu'[#id], verb.repair('shrdlu'[#id], 'shrdlu'[#id])))");
                this.shrdluSays("perf.farewell('player'[#id])");
                this.act_2_state = 108;
                this.game.shrdluAI.respondToPerformatives = false;
                break;
            case 108:
                if (this.act_2_state_timer >= 50 * 2 &&
                    this.game.shrdluAI.isIdle()) {
                    this.setupShrdluSelfRepairGoals();
                    this.act_2_state = 109;
                }
                break;
            case 109:
                if (this.act_2_state_timer >= 50 * 2 &&
                    this.game.etaoinAI.isIdle()) {
                    var term_h = Term.fromString("verb.need(E:'etaoin'[#id], verb.help(D:'player'[#id], E, verb.find(D, 'shrdlu'[#id])))", this.game.ontology);
                    this.game.etaoinAI.addLongTermTermWithSign(term_h, MEMORIZE_PROVENANCE, false);
                    term_h = Term.fromString("verb.need(S:'shrdlu'[#id], verb.help(D:'player'[#id], S, verb.take-to(D, 'shrdlu'[#id], 'location-aurora-station'[#id])))", this.game.ontology);
                    this.game.shrdluAI.addLongTermTermWithSign(term_h, MEMORIZE_PROVENANCE, false);
                    this.etaoinSays("perf.thankyou('player'[#id])");
                    this.act_2_state = 110;
                }
                break;
            case 110:
                if (this.game.etaoinAI.isIdle()) {
                    this.etaoinSays("perf.inform('player'[#id], #and(X:verb.repair('shrdlu'[#id], 'etaoin-memory'[#id]), time.future(X)))");
                    this.act_2_state = 111;
                }
                break;
            case 111:
                if (this.game.etaoinAI.isIdle()) {
                    this.queueThoughtBubble("Ok, it seems Shrdlu needs to fix itself first. So, repairs might take a while...");
                    this.queueThoughtBubble("I could go explore outside a bit more, now that I have the rover... or maybe I could just have a nap...");
                    this.act_2_state = 112;
                }
                break;
            case 112:
                // waiting for Shrdlu to repair Etaoin's memory
                if (this.game.shrdluAI.goals.length == 0) {
                    this.act_2_state = 201;
                }
                else if (this.game.currentPlayer.sleepingInBed != null) {
                    this.act_2_state = 120;
                }
                break;
            case 120:
                if (this.act_2_state_timer == 0) {
                    this.game.eyesClosedState = 3;
                    this.game.eyesClosedTimer = 0;
                    this.game.textInputAllowed = false;
                }
                else if (this.act_2_state_timer >= EYESOPEN_SPEED + 60) {
                    // fast forward the state:
                    this.game.shrdluAI.respondToPerformatives = true;
                    this.game.shrdluAI.visionActive = true;
                    this.game.shrdluAI.clearCurrentAction();
                    this.game.shrdluAI.clearScriptQueues();
                    this.contextShrdlu.inConversation = false;
                    this.game.requestWarp(this.game.shrdluAI.robot, this.game.qwertyAI.robot.map, 91 * 8, 22 * 8);
                    this.game.shrdluAI.robot.x = 91 * 8; // we force anyway, since, otehrwise, the agenda doesn't work
                    this.game.shrdluAI.robot.y = 22 * 8;
                    this.game.shrdluAI.addLongTermTerm(Term.fromString("verb.do('shrdlu'[#id], verb.repair('shrdlu'[#id], 'etaoin'[#id]))", this.game.ontology), PERCEPTION_PROVENANCE);
                    for (var _i = 0, _a = [this.game.etaoinAI, this.game.qwertyAI, this.game.shrdluAI]; _i < _a.length; _i++) {
                        var ai = _a[_i];
                        var se = ai.longTermMemory.findSentenceEntry(Sentence.fromString("property.broken('broken-stasis-pod'[#id])", this.game.ontology));
                        if (se != null)
                            se.sentence.sign[0] = false;
                    }
                    this.game.shrdluAI.robot.strength = 8; // increase Shrdlu's strength
                    this.act_2_state = 121;
                }
                break;
            case 121:
                if (this.act_2_state_timer == 0) {
                    this.game.in_game_seconds += 3600 * 8; // sleeping for 8 hours
                    this.game.narrationMessages.push("Several hours later...");
                    this.game.currentPlayer.x = 560;
                    this.game.currentPlayer.y = 216;
                    this.game.currentPlayer.direction = A4_DIRECTION_DOWN;
                }
                else if (this.act_2_state_timer == 120) {
                    this.game.narrationMessages = [];
                }
                else if (this.act_2_state_timer == 180) {
                    this.game.eyesClosedState = 1;
                    this.game.eyesClosedTimer = 0;
                    this.game.textInputAllowed = false;
                }
                else if (this.act_2_state_timer == 180 + EYESOPEN_SPEED) {
                    this.game.currentPlayer.state = A4CHARACTER_STATE_IN_BED;
                    this.game.textInputAllowed = true;
                    this.act_2_state = 201;
                    var term_3 = Term.fromString("verb.wake-up('player'[#id])", this.game.ontology);
                    this.game.etaoinAI.addLongTermTerm(term_3, PERCEPTION_PROVENANCE);
                    this.game.qwertyAI.addLongTermTerm(term_3, PERCEPTION_PROVENANCE);
                    this.game.shrdluAI.addLongTermTerm(term_3, PERCEPTION_PROVENANCE);
                }
                break;
            case 201:
                if (this.game.etaoinAI.isIdle()) {
                    this.game.etaoinAI.loadLongTermRulesFromFile("data/additional-kb-memoryrepair.xml");
                    this.game.qwertyAI.loadLongTermRulesFromFile("data/additional-kb-memoryrepair.xml");
                    this.game.shrdluAI.loadLongTermRulesFromFile("data/additional-kb-memoryrepair.xml");
                    this.etaoinSays("perf.inform('player'[#id], #and(X:verb.repair('shrdlu'[#id], 'etaoin-memory'[#id]), time.past(X)))");
                    this.act_2_state = 202;
                    this.setupShrdluRepairCommsGoals();
                }
                break;
            case 202:
                if (this.game.etaoinAI.isIdle()) {
                    this.etaoinSays("perf.inform('player'[#id], #and(X:erased('etaoin-memory'[#id]), time.past(X, time.date('42956019000'[number], [time.day]))))");
                    this.etaoinSays("perf.inform('player'[#id], #and(X:verb.repair('shrdlu'[#id], 'location-aurora-station'[#id]), time.subsequently(X)))");
                    this.act_2_state = 210;
                }
                break;
            case 210:
                // waiting for Shrdlu to repair the comm tower:
                if (this.game.shrdluAI.goals.length == 0) {
                    this.act_2_state = 211;
                }
                break;
            case 211:
                if (this.game.etaoinAI.isIdle()) {
                    this.etaoinSays("perf.inform('player'[#id], #and(X:verb.repair('shrdlu'[#id], 'comm-tower'[#id]), time.past(X)))");
                    this.setupShrdluAgendaGoals();
                    this.act_2_state = 212;
                }
                break;
            case 212:
                if (this.game.etaoinAI.isIdle()) {
                    this.updateKnowledgeAfterRepairingCommTower();
                    this.etaoinSays("perf.inform('player'[#id], verb.detect('etaoin'[#id], #and(V:[distress-signal], plural(V))))");
                    this.etaoinSays("perf.request.action('player'[#id], #and(V1:verb.go-to('player'[#id], 'location-as29'[#id]), relation.purpose(V1, verb.investigate('player'[#id]))))");
                    this.etaoinSays("perf.inform('player'[#id], verb.have('qwerty'[#id], 'command-key'[#id]))");
                    this.act_2_state = 220;
                }
                break;
            case 220:
                // waiting for David to enter the command center:
                if (this.game.currentPlayer.isIdle()) {
                    var currentRoom = this.game.getAILocation(this.game.currentPlayer);
                    // "location-as29" is the command center
                    if (currentRoom != null && currentRoom.id == "location-as29") {
                        this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "This is the command center of the whole station. It even has a map of the whole area!", A4_DIRECTION_NONE, this.game);
                        this.act_2_state = 221;
                    }
                }
                break;
            case 221:
                // waiting for David to examine the tactical map
                if (this.game.getStoryStateVariable("distress-signals") == "seen") {
                    // put the first distress signal in the mentions of etaoin, so that he would disambiguate to it:
                    this.contextEtaoin.addMention("distress-signal1", this.game.etaoinAI.timeStamp, this.game.etaoinAI.o);
                    if (this.player_has_asked_to_take_shrdlu) {
                        this.queueThoughtBubble("Maybe now Etaoin will let me take Shrdlu with me to investigate...");
                    }
                    this.act_2_state = 222;
                }
                break;
            case 222:
                // Waiting for the player to reach the crash site:
                if (this.game.currentPlayer.map.name == "Spacer Gorge" &&
                    this.game.currentPlayer.x < 256) {
                    // reached the crash site:
                    this.queueThoughtBubble("Oh my god, this is a crash site! there are lots of bodies here!");
                    this.queueThoughtBubble("Who are these people? and what happened here?");
                    this.queueThoughtBubble("Let's look around to see if I find any clues...");
                    this.act_2_state = 223;
                }
                break;
            case 223:
                // end of act 2!
                if (this.game.currentPlayer.isIdle() &&
                    this.game.getStoryStateVariable("act") == "act3") {
                    this.game.introact_request = 3;
                    this.app.achievement_complete_act2 = true;
                    this.app.trigger_achievement_complete_alert();
                }
                break;
        }
        if (this.playerAskedAboutTakingShrdlu() != null) {
            if (this.player_has_asked_to_take_shrdlu) {
                this.etaoinSays("perf.inform('player'[#id], #and(X:permission-to(V3:'player'[#id], action.take('player'[#id], 'shrdlu'[#id])), time.already(X)))");
            }
            else {
                // The player has asked to take SHRDLU
                if (this.act_2_state < 107) {
                    // we have not yet found SHRDLU:
                    this.etaoinSays("perf.inform('player'[#id], #not(space.at('shrdlu'[#id], 'location-aurora-station'[#id])))");
                }
                else if (this.act_2_state < 222) {
                    // too early, Etaoin rejects:
                    this.etaoinSays("perf.inform.answer('player'[#id], 'no'[symbol])");
                }
                else {
                    // We have found the distress signals, Etaoin accepts:
                    this.etaoinSays("perf.ack.ok('player'[#id])");
                    this.game.etaoinAI.addLongTermTerm(Term.fromString("permission-to(V3:'player'[#id], action.take('player'[#id], 'shrdlu'[#id]))", this.game.ontology), PERCEPTION_PROVENANCE);
                    this.game.setStoryStateVariable("permission-to-take-shrdlu", "true");
                    this.player_has_asked_to_take_shrdlu = true;
                }
            }
        }
        // this.shrdluAct2AgendaUpdate();
        this.repairDatapadUpdate();
        if (previous_state == this.act_2_state) {
            this.act_2_state_timer++;
        }
        else {
            this.act_2_state_timer = 0;
            this.act_2_state_start_time = this.game.in_game_seconds;
        }
        previous_state = this.act_2_repair_shuttle_state;
        switch (this.act_2_repair_shuttle_state) {
            case 0:
                if (this.playerAsksShrdluToFix("garage-shuttle")) {
                    var thingToRepairObject = this.game.findObjectByIDJustObject("garage-shuttle");
                    if (this.game.shrdluAI.canSee("garage-shuttle")) {
                        if (thingToRepairObject.sort.name == "brokenshuttle") {
                            // if shrdlu has the engine, it's all ok, otherwise, he cannot repair it:
                            var l = this.game.shrdluAI.robot.findObjectByID("shuttle-engine");
                            if (l != null && l.length == 1) {
                                // SHRDLU has the engine:
                                this.shrdluSays("perf.ack.ok('player'[#id])");
                                this.act_2_repair_shuttle_state = 1;
                            }
                            else {
                                // SHRDLU does not have the engine, it cnanot repair:
                                this.shrdluSays("perf.ack.denyrequest('player'[#id])");
                                this.shrdluSays("perf.inform('player'[#id], #not(verb.have('shrdlu'[#id], [shuttle-engine])))");
                            }
                        }
                        else {
                            this.shrdluSays("perf.inform('player'[#id], #not(property.broken('garage-shuttle'[#id])))");
                        }
                    }
                    else {
                        this.shrdluSays("perf.inform('player'[#id], #not(verb.see('shrdlu'[#id], [shuttle])))");
                    }
                }
                break;
            case 1:
                if (this.act_2_repair_shuttle_state_timer == 0) {
                    this.shrdluMoves(106 * 8, 28 * 8, this.game.maps[0]);
                    this.game.shrdluAI.respondToPerformatives = false;
                }
                else if (this.act_2_repair_shuttle_state_timer > 60 * 60) {
                    // if a whole minute has passed, something went wrong, reset!
                    this.act_2_repair_shuttle_state = 0;
                    this.game.shrdluAI.respondToPerformatives = true;
                }
                else {
                    if (this.game.shrdluAI.robot.x == 106 * 8 &&
                        this.game.shrdluAI.robot.y == 28 * 8) {
                        // lose the engine:
                        var l = this.game.shrdluAI.robot.findObjectByID("shuttle-engine");
                        var shuttle = this.game.findObjectByIDJustObject("garage-shuttle");
                        if (l != null && l.length == 1 && shuttle != null) {
                            this.game.shrdluAI.robot.removeFromInventory(l[0]);
                            // swap the shuttle:
                            shuttle.map.removeObject(shuttle);
                            this.game.requestDeletion(shuttle);
                            var newShuttle = this.game.objectFactory.createObject("garage-shuttle", this.game, true, false);
                            if (newShuttle == null) {
                                // something went wrong, reset!
                                this.act_2_repair_shuttle_state = 0;
                            }
                            else {
                                newShuttle.ID = shuttle.ID;
                                newShuttle.direction = 2;
                                var map = this.game.getMap("Aurora Station");
                                if (map == null)
                                    return false;
                                newShuttle.warp(shuttle.x, shuttle.y, map);
                                this.game.playSound("data/sfx/itemPickup.wav");
                                // done:
                                this.act_2_repair_shuttle_state = 2;
                                this.game.shrdluAI.respondToPerformatives = true;
                                this.game.shrdluAI.clearCurrentAction();
                            }
                        }
                        else {
                            // something went wrong, reset!
                            this.act_2_repair_shuttle_state = 0;
                            this.game.shrdluAI.respondToPerformatives = true;
                        }
                    }
                }
                break;
        }
        if (previous_state == this.act_2_repair_shuttle_state) {
            this.act_2_repair_shuttle_state_timer++;
        }
        else {
            this.act_2_repair_shuttle_state_timer = 0;
        }
    };
    ShrdluGameScript.prototype.updateKnowledgeAfterRepairingCommTower = function () {
        this.game.etaoinAI.addLongTermRuleNow(Sentence.fromString("~space.at(X,'aurora'[#id]);space.at(X,'communicator-range'[#id])", this.game.ontology), PERCEPTION_PROVENANCE);
        this.game.etaoinAI.addLongTermTerm(Term.fromString("distress-signal('distress-signal1'[#id])", this.game.ontology), PERCEPTION_PROVENANCE);
        this.game.etaoinAI.addLongTermTerm(Term.fromString("distress-signal('distress-signal2'[#id])", this.game.ontology), PERCEPTION_PROVENANCE);
        this.game.etaoinAI.addLongTermTerm(Term.fromString("space.inside.of('distress-signal1'[#id],'spacer-gorge'[#id])", this.game.ontology), PERCEPTION_PROVENANCE);
        this.game.etaoinAI.addLongTermTerm(Term.fromString("space.inside.of('distress-signal2'[#id],'trantor-crater'[#id])", this.game.ontology), PERCEPTION_PROVENANCE);
        this.game.etaoinAI.addLongTermTerm(Term.fromString("verb.come-from('distress-signal1'[#id],'spacer-gorge'[#id])", this.game.ontology), PERCEPTION_PROVENANCE);
        this.game.etaoinAI.addLongTermTerm(Term.fromString("verb.come-from('distress-signal2'[#id],'trantor-crater'[#id])", this.game.ontology), PERCEPTION_PROVENANCE);
        this.game.etaoinAI.addLongTermTerm(Term.fromString("goal(D:'player'[#id], verb.investigate(X, 'distress-signal1'[#id]))", this.game.ontology), PERCEPTION_PROVENANCE);
        var idx = this.game.qwertyAI.objectsNotAllowedToGive.indexOf("command-key");
        this.game.qwertyAI.objectsNotAllowedToGive.splice(idx, 1);
        this.game.shrdluAI.allowPlayerInto("location-as29", "COMMAND");
        this.game.qwertyAI.allowPlayerInto("location-as29", "COMMAND");
        this.game.etaoinAI.allowPlayerInto("location-as29", "COMMAND");
        this.game.comm_tower_repaired = true;
    };
    ShrdluGameScript.prototype.repairDatapadUpdate = function () {
        var request = this.playerAskedToRepairTheDatapad();
        var requestee = null;
        if (request != null && (request.attributes[0] instanceof ConstantTermAttribute)) {
            requestee = (request.attributes[0]).value;
            // if asked to Shrdlu, say it is too delicate:
            if (requestee == "shrdlu") {
                this.shrdluSays("perf.inform('player'[#id], too-small('shuttle-datapad'[#id]))");
            }
        }
        var previous_state = this.act_2_datapad_state;
        switch (this.act_2_datapad_state) {
            case 0: // no progress in this thread yet
                // detect when the player has asked qwerty to fix the broken space suit:
                if (request != null && requestee == "qwerty") {
                    var target_l_2 = this.game.findObjectByID("shuttle-datapad");
                    var weCanGetIt = false;
                    if (target_l_2 != null && target_l_2.length == 1 &&
                        this.game.qwertyAI.canSee("shuttle-datapad"))
                        weCanGetIt = true;
                    if (target_l_2 != null && target_l_2.length == 2 &&
                        (target_l_2[0] == this.game.qwertyAI.robot ||
                            target_l_2[0] == this.game.currentPlayer))
                        weCanGetIt = true;
                    if (weCanGetIt) {
                        this.act_2_datapad_state = 1;
                    }
                    else {
                        this.act_2_datapad_state = 0;
                        this.game.qwertyAI.respondToPerformatives = true;
                        // I do not see the datapad:
                        this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, #not(verb.see($QWERTY, 'shuttle-datapad'[#id]))))");
                    }
                }
                break;
            // qwerty has been asked to repair the datapad
            case 1:
                if (this.act_2_datapad_state_timer == 0) {
                    this.game.qwertyAI.respondToPerformatives = false; // to prevent the player messing up with the sequence
                    this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, #and(V:verb.repair($QWERTY, 'shuttle-datapad'[#id]), time.future(V)) ))");
                    // clear whatever qwerty is doing now:
                    this.game.qwertyAI.clearCurrentAction();
                    var currentRoom = this.game.getAILocation(this.game.qwertyAI.robot);
                    if (currentRoom.id != "location-as29") {
                        // if we are not in the command center, qwerty asks the player to follow it:
                        this.qwertyIntention("action.talk($QWERTY, perf.request.action(V0:$PLAYER, verb.follow(V0, $QWERTY)))");
                    }
                }
                else {
                    if (this.game.qwertyAI.isIdle())
                        this.act_2_datapad_state = 2;
                }
                break;
            // go towards david to take the datapad:
            case 2:
                var target_l = this.game.findObjectByID("shuttle-datapad");
                if (target_l == null) {
                    this.act_2_datapad_state = 0;
                    this.game.qwertyAI.respondToPerformatives = true;
                }
                else {
                    var d_2 = this.game.qwertyAI.robot.pixelDistance(target_l[0]);
                    if (target_l.length == 1) {
                        // the spacesuit is in the floor:
                        if (d_2 > 0) {
                            this.game.qwertyAI.robot.AI.addPFTargetObject(A4CHARACTER_COMMAND_IDLE, 10, false, target_l[0], this.game);
                        }
                        else {
                            if (this.game.qwertyAI.robot.takeAction(this.game)) {
                                this.act_2_datapad_state = 3;
                            }
                            else {
                                this.act_2_datapad_state = 0;
                                this.game.qwertyAI.respondToPerformatives = true;
                            }
                        }
                    }
                    else {
                        // someone has the spacesuit (this could be qwerty itself):
                        if (d_2 > 16) {
                            this.game.qwertyAI.robot.AI.addPFTargetObject(A4CHARACTER_COMMAND_IDLE, 10, false, target_l[0], this.game);
                        }
                        else {
                            if (target_l[0] instanceof A4Character) {
                                // assume datapad is target_l[1]!!!
                                (target_l[0]).removeFromInventory(target_l[1]);
                                this.game.qwertyAI.robot.addObjectToInventory(target_l[1], this.game);
                                this.act_2_datapad_state = 3;
                                this.game.playSound("data/sfx/itemPickup.wav");
                            }
                            else {
                                this.act_2_datapad_state = 0;
                                this.game.qwertyAI.respondToPerformatives = true;
                            }
                        }
                    }
                }
                break;
            case 3:
                // qwerty has the datapad!:
                if (this.act_2_datapad_state_timer == 0) {
                    this.qwertyMoves(107 * 8, 37 * 8, this.game.qwertyAI.robot.map);
                }
                else {
                    if (this.game.qwertyAI.robot.x == 107 * 8 &&
                        this.game.qwertyAI.robot.y == 37 * 8) {
                        this.act_2_datapad_state = 4;
                    }
                }
                break;
            case 4:
                // wait a bit
                if (this.act_2_datapad_state_timer == 180)
                    this.act_2_datapad_state = 5;
                break;
            case 5:
                // give the datapad back to the player
                var d_3 = this.game.qwertyAI.robot.pixelDistance(this.game.currentPlayer);
                if (d_3 > 16) {
                    this.game.qwertyAI.robot.AI.addPFTargetObject(A4CHARACTER_COMMAND_IDLE, 10, false, this.game.currentPlayer, this.game);
                }
                else {
                    // give the datapad:
                    var idx = 0;
                    for (var i = 0; i < this.game.qwertyAI.robot.inventory.length; i++) {
                        if (this.game.qwertyAI.robot.inventory[i].ID == "shuttle-datapad") {
                            idx = i;
                            break;
                        }
                    }
                    this.game.qwertyAI.robot.inventory.splice(idx, 1);
                    var fixedDatapad = this.game.objectFactory.createObject("fixed-datapad", this.game, false, false);
                    fixedDatapad.ID = "shuttle-datapad";
                    this.game.currentPlayer.addObjectToInventory(fixedDatapad, this.game);
                    // replace the background knowledge:
                    for (var _i = 0, _a = [this.game.etaoinAI, this.game.qwertyAI, this.game.shrdluAI]; _i < _a.length; _i++) {
                        var ai = _a[_i];
                        var se = ai.longTermMemory.findSentenceEntry(Sentence.fromString("shuttle-datapad('shuttle-datapad'[#id])", this.game.ontology));
                        if (se != null)
                            se.sentence.terms[0].functor = this.game.ontology.getSort("fixed-datapad");
                    }
                    this.game.currentPlayer.map.addPerceptionBufferRecord(new PerceptionBufferRecord("give", this.game.qwertyAI.robot.ID, this.game.qwertyAI.robot.sort, this.game.currentPlayer.ID, this.game.currentPlayer.sort, null, fixedDatapad.ID, fixedDatapad.sort, this.game.qwertyAI.robot.x, this.game.qwertyAI.robot.y, this.game.qwertyAI.robot.x + this.game.qwertyAI.robot.getPixelWidth(), this.game.qwertyAI.robot.y + this.game.qwertyAI.robot.getPixelHeight()));
                    this.game.currentPlayer.eventWithObject(A4_EVENT_RECEIVE, this.game.qwertyAI.robot, fixedDatapad, this.game.currentPlayer.map, this.game);
                    this.game.qwertyAI.robot.eventWithObject(A4_EVENT_ACTION_GIVE, this.game.currentPlayer, fixedDatapad, this.game.currentPlayer.map, this.game);
                    this.game.playSound("data/sfx/itemPickup.wav");
                    this.game.inGameActionsForLog.push(["give(" + this.game.qwertyAI.selfID + "," + fixedDatapad.ID + "," + this.game.currentPlayer.ID + ")", "" + this.game.in_game_seconds]);
                    this.game.qwertyAI.respondToPerformatives = true;
                    this.act_2_datapad_state = 0;
                    this.qwertyIntention("action.talk($QWERTY, perf.inform($PLAYER, #and(V:verb.salvage('etaoin'[#id], #and(some(X:'entry'[symbol], [plural]), noun(X, [plural]))), time.past(V))))");
                    this.game.etaoinAI.loadLongTermRulesFromFile("data/additional-kb-datapad.xml");
                    this.game.qwertyAI.loadLongTermRulesFromFile("data/additional-kb-datapad.xml");
                    this.game.shrdluAI.loadLongTermRulesFromFile("data/additional-kb-datapad.xml");
                }
                break;
        }
        if (previous_state == this.act_2_datapad_state) {
            this.act_2_datapad_state_timer++;
        }
        else {
            this.act_2_datapad_state_timer = 0;
        }
    };
    /* --------------------------------------------------------
        ACT 3: AURORA
       -------------------------------------------------------- */
    ShrdluGameScript.prototype.update_act_3 = function () {
        var previous_state = this.act_3_state;
        switch (this.act_3_state) {
            case 0:
                this.game.loadTardis8LocationKnowledge();
                this.updateKnowledgeAfterReachingTrantorCrater();
                this.act_3_state = 1;
                break;
            case 1:
                if (this.game.currentPlayer.map.name == "Tardis 8") {
                    this.queueThoughtBubble("I don't know how safe will it be to walk through these halls but let's investigate!");
                    this.queueThoughtBubble("If the computer is in a working state, it can hold the key to who am I, and what happened here!");
                    // If we have given a name, question it:
                    var name_3 = this.getNameGivenToTheAI();
                    if (name_3 != null) {
                        this.queueThoughtBubble("Am I really " + name_3 + "?");
                    }
                    this.act_3_state = 2;
                }
                break;
            case 2: // waiting for shrdlu to repair the lights on the ship
                if (this.act_3_repair_tardis_console_state >= 3) {
                    this.queueThoughtBubble("Alright! now we are talking!");
                    this.act_3_state = 3;
                }
                break;
            case 3: // waiting to enter one of the stasis rooms:
                if (this.game.currentPlayer.map.name == "Tardis 8" &&
                    this.game.currentPlayer.y < 11 * 8 ||
                    this.game.currentPlayer.y > 21 * 8) {
                    this.queueThoughtBubble("Oh! A giant stasis chamber! There must be hundreds of corpses in this ship!");
                    this.queueThoughtBubble("What an awful sight! These must be the colonists that were suposed to populate Aurora...");
                    this.act_3_state = 4;
                }
                break;
            case 4: // waiting to enter the engineering bay:
                if (this.game.currentPlayer.map.name == "Tardis 8" &&
                    this.game.currentPlayer.x < 65 * 8) {
                    this.queueThoughtBubble("This looks like an engineering bay of some sort. Most machines are destroyed...");
                    this.queueThoughtBubble("But there is a lot of suplies here that could be useful...");
                    this.act_3_state = 5;
                }
                break;
            case 5: // waiting to enter the bridge:
                if (this.game.currentPlayer.map.name == "Tardis 8" &&
                    this.game.currentPlayer.x < 18 * 8) {
                    this.queueThoughtBubble("at last! the bridge! Let's see what can I learn here!");
                    this.queueThoughtBubble("Also, the central computer should be somewhere here, what would its memory contain?!");
                    this.act_3_state = 6;
                }
                break;
            case 6: // waiting to enter the computer room:
                if (this.game.currentPlayer.map.name == "Tardis 8" &&
                    this.game.currentPlayer.x < 18 * 8 &&
                    this.game.currentPlayer.y < 14 * 8) {
                    this.queueThoughtBubble("Finally! This is the computer room!");
                    this.queueThoughtBubble("I might be able to take a memory core and bring it to the station for analysis!");
                    this.act_3_state = 7;
                }
                break;
            case 7: // waiting for the player to place the memory core in a console:
                if (this.game.getStoryStateVariable("tardis-memory-core") == "inconsole" &&
                    this.game.currentPlayer.isIdle()) {
                    this.etaoinSays("perf.inform(D:'player'[#id], verb.read('etaoin'[#id], 'tardis-memory-core'[#id]))");
                    this.act_3_state = 8;
                }
                break;
            case 8:
                if (this.game.etaoinAI.isIdle()) {
                    // make sure Etaoin knows about this object, that has disappeared from the game:
                    this.game.etaoinAI.loadLongTermRulesFromFile("data/additional-kb-ending-" + this.game.playerGender + ".xml");
                    this.game.qwertyAI.loadLongTermRulesFromFile("data/additional-kb-ending-" + this.game.playerGender + ".xml");
                    this.game.shrdluAI.loadLongTermRulesFromFile("data/additional-kb-ending-" + this.game.playerGender + ".xml");
                    this.game.etaoinAI.allowPlayerIntoEveryWhere();
                    this.game.qwertyAI.allowPlayerIntoEveryWhere();
                    this.game.shrdluAI.allowPlayerIntoEveryWhere();
                    if (this.game.playerGender == "male") {
                        this.etaoinSays("perf.inform(D:'player'[#id], name('player'[#id],'david bowman'[symbol]))");
                    }
                    else {
                        this.etaoinSays("perf.inform(D:'player'[#id], name('player'[#id],'susan calvin'[symbol]))");
                    }
                    this.etaoinSays("perf.inform(D:'player'[#id], role('player'[#id],'location-aurora-station'[#id],'computer-engineer'[computer-engineer]))");
                    this.etaoinSays("perf.inform(D:'player'[#id], #and(X:action.give('qwerty'[#id], [datapad], 'player'[#id]), time.future(X)))");
                    var datapad = this.game.objectFactory.createObject("final-datapad", this.game, false, false);
                    datapad.ID = "final-datapad";
                    this.game.qwertyAI.robot.inventory.push(datapad);
                    this.act_3_state = 9;
                }
                break;
            case 9:
                if (this.game.etaoinAI.intentions.length == 0 &&
                    this.game.etaoinAI.queuedIntentions.length == 0) {
                    // make Qwerty do it:
                    this.game.qwertyAI.intentions = [];
                    this.qwertyIntention("action.give('qwerty'[#id], 'final-datapad'[#id], 'player'[#id])");
                    this.act_3_state = 10;
                }
                break;
            case 10: // Qwerty will bring a datapad, and the master key to David:
                // If nothing has happened for a while, re-ask qwerty to do it ...
                if (this.game.qwertyAI.robot.findObjectByID("final-datapad") == null) {
                    this.act_3_state = 11;
                }
                else if (this.act_3_state_start_time > 500) {
                    this.game.qwertyAI.intentions = [];
                    this.qwertyIntention("action.give('qwerty'[#id], 'final-datapad'[#id], 'player'[#id])");
                }
                break;
            case 11: // give the master key to the player:
                var key = this.game.objectFactory.createObject("master-key", this.game, false, false);
                key.ID = "player-masterkey";
                this.game.qwertyAI.robot.inventory.push(key);
                this.game.qwertyAI.intentions = [];
                if (!this.contextQwerty.inConversation) {
                    this.qwertyIntention("action.talk('qwerty'[#id], perf.callattention('player'[#id]))");
                }
                this.act_3_state = 12;
                break;
            case 12:
                if (this.game.qwertyAI.isIdle()) {
                    this.qwertyIntention("action.talk('qwerty'[#id], perf.request.action('player'[#id], action.take('player'[#id], 'player-masterkey'[#id])))");
                    this.act_3_state = 13;
                }
            case 13:
                if (this.game.qwertyAI.isIdle()) {
                    this.qwertyIntention("action.give('qwerty'[#id], 'player-masterkey'[#id], 'player'[#id])");
                    this.act_3_state = 14;
                }
                break;
            case 14:
                if (this.game.currentPlayer.findObjectByID("player-masterkey") != null) {
                    this.queueThoughtBubble("Wow... so, I was a computer engineer here... I do not remember anything...");
                    this.queueThoughtBubble("After all this work, this is it. This datapad contains the information I wanted... I now just have to read it...");
                    this.queueThoughtBubble("Also, it seems etaoin recognized me now, and I have access to everywhere in the station, which is convenient...");
                    this.act_3_state = 15;
                }
                break;
            case 15: // story events are over! now just waiting for the player to finish the game...
                break;
        }
        if (previous_state == this.act_3_state) {
            this.act_3_state_timer++;
        }
        else {
            this.act_3_state_timer = 0;
            this.act_3_state_start_time = this.game.in_game_seconds;
        }
        previous_state = this.act_3_repair_tardis_console_state;
        switch (this.act_3_repair_tardis_console_state) {
            case 0:
                if (this.playerAsksShrdluToFix("tardis-wall-computer")) {
                    if (this.game.shrdluAI.canSee("tardis-wall-computer")) {
                        this.act_3_repair_tardis_console_state = 1;
                        this.shrdluSays("perf.ack.ok('player'[#id])");
                    }
                    else {
                        this.shrdluSays("perf.inform('player'[#id], #not(verb.see('shrdlu'[#id], [tardis-wall-computer])))");
                    }
                }
                break;
            case 1:
                if (this.act_3_repair_tardis_console_state_timer == 0) {
                    this.shrdluMoves(85 * 8, 16 * 8, this.game.maps[8]);
                }
                else {
                    if (this.game.shrdluAI.robot.x == 85 * 8 &&
                        this.game.shrdluAI.robot.y == 16 * 8) {
                        // arrived:
                        this.act_3_repair_tardis_console_state = 2;
                    }
                }
                break;
            case 2:
                if (this.act_3_repair_tardis_console_state_timer >= 100) {
                    // turn the lights on:
                    this.game.setStoryStateVariable("tardis-8-lights", "on");
                    this.game.turnLightOn("tardis8-corridor-east");
                    this.game.turnLightOn("tardis8-stasis1");
                    this.game.turnLightOn("tardis8-stasis2");
                    this.game.turnLightOn("tardis8-engineering");
                    this.game.findObjectByIDJustObject("tardis-east-door-1").canBeOpen = true;
                    this.game.findObjectByIDJustObject("tardis-east-door-2").canBeOpen = true;
                    this.game.findObjectByIDJustObject("tardis-east-door-3").canBeOpen = true;
                    this.game.findObjectByIDJustObject("tardis-east-door-4").canBeOpen = true;
                    this.act_3_repair_tardis_console_state = 3;
                }
                break;
        }
        if (previous_state == this.act_3_repair_tardis_console_state) {
            this.act_3_repair_tardis_console_state_timer++;
        }
        else {
            this.act_3_repair_tardis_console_state_timer = 0;
        }
        previous_state = this.act_3_repair_tardis_cable_state;
        switch (this.act_3_repair_tardis_cable_state) {
            case 0:
                if (this.playerAsksShrdluToFix("tardis-broken-cable")) {
                    if (this.game.shrdluAI.canSee("tardis-broken-cable")) {
                        if (this.game.shrdluAI.robot.findObjectByName("extension cord") != null) {
                            this.shrdluSays("perf.ack.ok('player'[#id])");
                            this.act_3_repair_tardis_cable_state = 1;
                        }
                        else if (this.game.shrdluAI.robot.findObjectByName("cable") != null) {
                            this.shrdluSays("perf.inform('player'[#id], #and(verb.need('shrdlu'[#id], X:[extension-cord]), big(X)))");
                        }
                        else {
                            this.shrdluSays("perf.inform('player'[#id], verb.need('shrdlu'[#id], [extension-cord]))");
                        }
                    }
                    else {
                        this.shrdluSays("perf.inform('player'[#id], #not(verb.see('shrdlu'[#id], [tardis-broken-cable])))");
                    }
                }
                break;
            case 1:
                if (this.act_3_repair_tardis_cable_state_timer == 0) {
                    this.shrdluMoves(45 * 8, 16 * 8, this.game.maps[8]);
                }
                else {
                    if (this.game.shrdluAI.robot.x == 45 * 8 &&
                        this.game.shrdluAI.robot.y == 16 * 8) {
                        // arrived:
                        this.act_3_repair_tardis_cable_state = 2;
                    }
                }
                break;
            case 2:
                if (this.act_3_repair_tardis_cable_state_timer >= 100) {
                    var thingToRepairObject = this.game.findObjectByIDJustObject("tardis-broken-cable");
                    thingToRepairObject.map.removeObject(thingToRepairObject);
                    this.game.requestDeletion(thingToRepairObject);
                    // change the graphic of the cable:
                    var map = this.game.getMap("Tardis 8");
                    map.layers[0].tiles[44 + 16 * map.layers[0].width] = this.game.mapTiles[661];
                    // map.layers[1].tiles[43+16*map.layers[0].width] = this.game.mapTiles[629];
                    map.layers[0].cacheDrawTiles();
                    map.layers[1].cacheDrawTiles();
                    this.game.setStoryStateVariable("tardis-8-lights-west", "on");
                    this.game.turnLightOn("tardis8-corridor-west");
                    this.game.turnLightOn("tardis8-bridge");
                    this.game.turnLightOn("tardis8-computer");
                    this.game.findObjectByIDJustObject("tardis-west-door-1").canBeOpen = true;
                    this.act_3_repair_tardis_cable_state = 3;
                }
                break;
        }
        if (previous_state == this.act_3_repair_tardis_cable_state) {
            this.act_3_repair_tardis_cable_state_timer++;
        }
        else {
            this.act_3_repair_tardis_cable_state_timer = 0;
        }
    };
    ShrdluGameScript.prototype.updateKnowledgeAfterReachingTrantorCrater = function () {
        this.game.etaoinAI.loadLongTermRulesFromFile("data/additional-kb-trantor.xml");
        this.game.qwertyAI.loadLongTermRulesFromFile("data/additional-kb-trantor.xml");
        this.game.shrdluAI.loadLongTermRulesFromFile("data/additional-kb-trantor.xml");
    };
    ShrdluGameScript.prototype.getNameGivenToTheAI = function () {
        for (var _i = 0, _a = this.game.etaoinAI.longTermMemory.plainSentenceList; _i < _a.length; _i++) {
            var s = _a[_i];
            if (s.sentence.terms.length == 1 && s.sentence.sign[0] &&
                s.sentence.terms[0].functor.name == "name" &&
                s.sentence.terms[0].attributes.length == 2 &&
                (s.sentence.terms[0].attributes[0] instanceof ConstantTermAttribute) &&
                (s.sentence.terms[0].attributes[1] instanceof ConstantTermAttribute)) {
                if (s.sentence.terms[0].attributes[0].value == "player") {
                    return s.sentence.terms[0].attributes[1].value;
                }
            }
        }
        return null;
    };
    /* --------------------------------------------------------
        EVENTS THAT ARE NOT TIED TO ANY PARTICULAR ACT
       -------------------------------------------------------- */
    ShrdluGameScript.prototype.actionRequestHandleByScript = function (action) {
        if (this.actionRequestIsAboutTakingShrdlu(action))
            return true;
        return false;
    };
    ShrdluGameScript.prototype.perfQPredicateHandleByScript = function (perf2) {
        if (perf2.attributes.length == 2 &&
            this.predicateIsAboutTakingShrdlu((perf2.attributes[1]).term))
            return true;
        return false;
    };
    ShrdluGameScript.prototype.update_sideplots = function () {
        // Finding life in Aurora subplot:
        if (!this.finding_life_side_plot_taken_question &&
            this.game.getStoryStateVariable("luminiscent-fungi") == "taken") {
            // Here if the player asks about finding life in aurora, he should be reminded to analyze the fungi with a thought bubble:
            if (this.playerAskedAboutFindingLife() != null) {
                this.queueThoughtBubble("We have not found life yet... But what about that weird dust I found?!");
                this.queueThoughtBubble("I need to find a way to analyze it! Could it be that this planet has developed life?!");
                this.finding_life_side_plot_taken_question = true;
            }
        }
        else if (!this.finding_life_side_plot_analyzed_question &&
            this.game.getStoryStateVariable("luminiscent-fungi") == "analyzed") {
            // Here, if the player asks about finding life in aurora, he should just be impressed by what he has found
            if (this.playerAskedAboutFindingLife() != null) {
                this.queueThoughtBubble("I still cannot believe that dust I found is a form of life!");
                this.queueThoughtBubble("How I wish there was some other person to share these amazing news with!");
                this.finding_life_side_plot_analyzed_question = true;
            }
        }
        if (!this.what_is_dust_side_plot_taken_question &&
            this.game.getStoryStateVariable("luminiscent-fungi") == "taken") {
            if (this.playerAskedAboutWhatIsTheDust()) {
                this.queueThoughtBubble("So, we don't know what is this dust...");
                this.queueThoughtBubble("I need to find a way to analyze it!!");
                this.what_is_dust_side_plot_taken_question = true;
            }
        }
    };
    ShrdluGameScript.prototype.playerAskedAboutFindingLife = function () {
        if (this.contextEtaoin == null ||
            this.contextQwerty == null ||
            this.contextShrdlu == null) {
            this.contextEtaoin = this.game.etaoinAI.contextForSpeaker(this.playerID);
            this.contextQwerty = this.game.qwertyAI.contextForSpeaker(this.playerID);
            this.contextShrdlu = this.game.shrdluAI.contextForSpeaker(this.playerID);
        }
        for (var _i = 0, _a = [this.contextQwerty, this.contextEtaoin, this.contextShrdlu]; _i < _a.length; _i++) {
            var context = _a[_i];
            if (context != null) {
                var p1 = context.lastPerformativeBy(this.playerID);
                var p2 = context.lastPerformativeBy(context.ai.selfID);
                if (p1 != null && p2 != null &&
                    p1.performative != null && p2.performative != null &&
                    p2.timeStamp == this.game.in_game_seconds - 1) {
                    var perf = p1.performative;
                    if (perf.functor.is_a(this.game.ontology.getSort("perf.q.predicate")) &&
                        perf.attributes.length > 1 &&
                        perf.attributes[1] instanceof TermTermAttribute) {
                        var argument = (perf.attributes[1]).term;
                        var pattern1 = Term.fromString("#and(verb.find(X, Y, Z), living-being(X))", this.game.ontology);
                        var pattern2 = Term.fromString("#and(verb.find(X, Y), living-being(X))", this.game.ontology);
                        var pattern3a = Term.fromString("#and(#not(=(X,'player'[#id])), #and(space.at(X, 'aurora'[#id]), living-being(X)))", this.game.ontology);
                        var pattern3b = Term.fromString("#and(space.at(X, 'aurora'[#id]), animal(X))", this.game.ontology);
                        if (pattern1.subsumes(argument, true, new Bindings()))
                            return perf;
                        if (pattern2.subsumes(argument, true, new Bindings()))
                            return perf;
                        if (pattern3a.subsumes(argument, true, new Bindings()) &&
                            !pattern3b.subsumes(argument, true, new Bindings()))
                            return perf;
                    }
                }
            }
        }
        return null;
    };
    ShrdluGameScript.prototype.playerAskedAboutWhatIsTheDust = function () {
        if (this.contextEtaoin == null ||
            this.contextQwerty == null ||
            this.contextShrdlu == null) {
            this.contextEtaoin = this.game.etaoinAI.contextForSpeaker(this.playerID);
            this.contextQwerty = this.game.qwertyAI.contextForSpeaker(this.playerID);
            this.contextShrdlu = this.game.shrdluAI.contextForSpeaker(this.playerID);
        }
        for (var _i = 0, _a = [this.contextQwerty, this.contextEtaoin, this.contextShrdlu]; _i < _a.length; _i++) {
            var context = _a[_i];
            if (context != null) {
                var p1 = context.lastPerformativeBy(this.playerID);
                var p2 = context.lastPerformativeBy(context.ai.selfID);
                if (p1 != null && p2 != null &&
                    p1.performative != null && p2.performative != null &&
                    p2.timeStamp == this.game.in_game_seconds - 1) {
                    var perf = p1.performative;
                    if (perf.functor.is_a(this.game.ontology.getSort("perf.q.whatis.noname")) &&
                        perf.attributes.length > 1 &&
                        perf.attributes[1] instanceof ConstantTermAttribute) {
                        var v = (perf.attributes[1]).value;
                        // These are the IDs of the luminiscent dust in West Cave
                        if (v == "1110" ||
                            v == "1111" ||
                            v == "1111" ||
                            v == "1113") {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    };
    ShrdluGameScript.prototype.questionAboutBeingAlone = function (perf) {
        var query = null;
        if (perf.functor.is_a(this.game.ontology.getSort("perf.q.query")) && perf.attributes.length == 3) {
            query = perf.attributes[2].term;
        }
        else if (perf.functor.is_a(this.game.ontology.getSort("perf.q.predicate")) ||
            perf.functor.is_a(this.game.ontology.getSort("perf.q.predicate-negated"))) {
            query = perf.attributes[1].term;
        }
        else if (perf.functor.is_a(this.game.ontology.getSort("perf.question")) &&
            perf.attributes.length >= 2 &&
            perf.attributes[1] instanceof TermTermAttribute) {
            query = perf.attributes[1].term;
        }
        if (query != null) {
            var queryTerms = Term.elementsInList(query, "#and");
            var humanFound = false;
            var differentFromPlayerFound = false;
            var patterna = Term.fromString("alone('player'[#id])", this.game.ontology);
            if (patterna.subsumes(query, true, new Bindings())) {
                return true;
            }
            for (var _i = 0, queryTerms_2 = queryTerms; _i < queryTerms_2.length; _i++) {
                var ta = queryTerms_2[_i];
                if (!(ta instanceof TermTermAttribute))
                    continue;
                var t = ta.term;
                if ((t.functor.name == "human" || t.functor.name == "character") &&
                    t.attributes.length == 1 &&
                    t.attributes[0] instanceof VariableTermAttribute) {
                    humanFound = true;
                }
                if (t.functor.name == "alone-in" &&
                    t.attributes[0] instanceof ConstantTermAttribute &&
                    t.attributes[1] instanceof ConstantTermAttribute &&
                    t.attributes[0].value == "player" &&
                    (t.attributes[1].value == "location-aurora-station" ||
                        t.attributes[1].value == "location-aurora-settlement" ||
                        t.attributes[1].value == "spacer-valley" ||
                        t.attributes[1].value == "aurora")) {
                    return true;
                }
                if (t.functor.name == "#not") {
                    var t2 = t.attributes[0].term;
                    if (t2.functor.name == "=" &&
                        t2.attributes.length == 2 &&
                        (t2.attributes[0] instanceof VariableTermAttribute) &&
                        (t2.attributes[1] instanceof ConstantTermAttribute) &&
                        t2.attributes[1].value == "player") {
                        differentFromPlayerFound = true;
                    }
                    if (t2.functor.name == "=" &&
                        t2.attributes.length == 2 &&
                        (t2.attributes[1] instanceof VariableTermAttribute) &&
                        (t2.attributes[0] instanceof ConstantTermAttribute) &&
                        t2.attributes[0].value == "player") {
                        differentFromPlayerFound = true;
                    }
                }
                if (t.functor.name == "!=" &&
                    t.attributes.length == 2) {
                    if ((t.attributes[1] instanceof VariableTermAttribute) &&
                        (t.attributes[0] instanceof ConstantTermAttribute) &&
                        t.attributes[0].value == "player") {
                        differentFromPlayerFound = true;
                    }
                    if ((t.attributes[0] instanceof VariableTermAttribute) &&
                        (t.attributes[1] instanceof ConstantTermAttribute) &&
                        t.attributes[1].value == "player") {
                        differentFromPlayerFound = true;
                    }
                }
            }
            if (humanFound && differentFromPlayerFound)
                return true;
        }
        return false;
    };
    ShrdluGameScript.prototype.playerChangedHerMindAfterSayingNo = function (ai) {
        var context = null;
        if (ai == "etaoin")
            context = this.contextEtaoin;
        if (ai == "shrdlu")
            context = this.contextShrdlu;
        if (context != null) {
            var p1 = context.lastPerformativeBy(this.playerID);
            if (p1 != null && p1.performative != null &&
                p1.timeStamp == this.game.in_game_seconds - 1) {
                var perf = p1.performative;
                if (perf.functor.is_a(this.game.ontology.getSort("perf.inform")) &&
                    perf.attributes.length > 1 &&
                    perf.attributes[1] instanceof TermTermAttribute) {
                    var argument = (perf.attributes[1]).term;
                    var pattern1 = Term.fromString("verb.can('player'[#id], verb.help('player'[#id], '" + ai + "'[#id]))", this.game.ontology);
                    var pattern2 = Term.fromString("verb.can('player'[#id], verb.help('player'[#id]))", this.game.ontology);
                    var b = new Bindings();
                    if (pattern1.subsumes(argument, true, b) ||
                        pattern2.subsumes(argument, true, b)) {
                        return true;
                    }
                }
                else if (perf.functor.is_a(this.game.ontology.getSort("perf.changemind"))) {
                    return true;
                }
            }
        }
        return false;
    };
    ShrdluGameScript.prototype.playerAsksQwertyToFixSpacesuit = function () {
        if (this.contextQwerty != null) {
            var p1 = this.contextQwerty.lastPerformativeBy(this.playerID);
            if (p1 != null && p1.performative != null &&
                p1.timeStamp == this.game.in_game_seconds - 1) {
                var perf = p1.performative;
                if (perf.functor.is_a(this.game.ontology.getSort("perf.inform")) &&
                    perf.attributes.length > 1 &&
                    perf.attributes[1] instanceof TermTermAttribute) {
                    var argument = (perf.attributes[1]).term;
                    var pattern1 = Term.fromString("property.broken('spacesuit'[#id])", this.game.ontology);
                    var pattern2 = Term.fromString("#and(verb.have(V1:'spacesuit'[#id], T:[#id]), tear(T))", this.game.ontology);
                    var b = new Bindings();
                    if (pattern1.subsumes(argument, true, b) ||
                        pattern2.subsumes(argument, true, b)) {
                        return true;
                    }
                }
                else if (perf.functor.is_a(this.game.ontology.getSort("perf.request.action")) &&
                    perf.attributes.length > 1 &&
                    perf.attributes[1] instanceof TermTermAttribute) {
                    var argument = (perf.attributes[1]).term;
                    var pattern3 = Term.fromString("verb.repair('qwerty'[#id], 'spacesuit'[#id])", this.game.ontology);
                    var b = new Bindings();
                    if (pattern3.subsumes(argument, true, b))
                        return true;
                }
            }
        }
        return false;
    };
    ShrdluGameScript.prototype.playerAsksShrdluToFix = function (objectId) {
        if (this.contextShrdlu != null) {
            var p1 = this.contextShrdlu.lastPerformativeBy(this.playerID);
            if (p1 != null && p1.performative != null &&
                p1.timeStamp == this.game.in_game_seconds - 1) {
                var perf = p1.performative;
                if (perf.functor.is_a(this.game.ontology.getSort("perf.request.action")) &&
                    perf.attributes.length > 1 &&
                    perf.attributes[1] instanceof TermTermAttribute) {
                    var argument = (perf.attributes[1]).term;
                    var pattern3 = Term.fromString("verb.repair('shrdlu'[#id], '" + objectId + "'[#id])", this.game.ontology);
                    var b = new Bindings();
                    if (pattern3.subsumes(argument, true, b))
                        return true;
                }
            }
        }
        return false;
    };
    ShrdluGameScript.prototype.playerAskedAboutTakingShrdlu = function () {
        if (this.contextEtaoin == null) {
            this.contextEtaoin = this.game.etaoinAI.contextForSpeaker(this.playerID);
        }
        var context = this.contextEtaoin;
        if (context != null) {
            var p1 = context.lastPerformativeBy(this.playerID);
            if (p1 != null && p1.performative != null && p1.timeStamp == this.game.in_game_seconds - 1) {
                var perf = p1.performative;
                if (perf.functor.is_a(this.game.ontology.getSort("perf.q.action")) ||
                    perf.functor.is_a(this.game.ontology.getSort("perf.request.action"))) {
                    var action = (perf.attributes[1]).term;
                    if (this.actionRequestIsAboutTakingShrdlu(action))
                        return perf;
                }
                else if (perf.functor.is_a(this.game.ontology.getSort("perf.q.predicate"))) {
                    var predicate = (perf.attributes[1]).term;
                    if (this.predicateIsAboutTakingShrdlu(predicate))
                        return perf;
                }
            }
        }
        return null;
    };
    ShrdluGameScript.prototype.actionRequestIsAboutTakingShrdlu = function (action) {
        var pattern1 = Term.fromString("action.take('player'[#id], 'shrdlu'[#id])", this.game.ontology);
        var pattern2 = Term.fromString("verb.take-to('player'[#id], 'shrdlu'[#id], LOCATION:[#id])", this.game.ontology);
        var pattern3 = Term.fromString("action.give('etaoin'[#id], 'player'[#id], permission-to(V3:'player'[#id], action.take('player'[#id], 'shrdlu'[#id])))", this.game.ontology);
        var pattern4 = Term.fromString("action.give('etaoin'[#id], 'shrdlu'[#id], permission-to(V3:'shrdlu'[#id], verb.leave('shrdlu'[#id])))", this.game.ontology);
        var pattern5 = Term.fromString("action.give('etaoin'[#id], 'shrdlu'[#id], permission-to(V3:'shrdlu'[#id], verb.leave('shrdlu'[#id], 'location-aurora-station'[#id])))", this.game.ontology);
        var pattern6 = Term.fromString("action.give('etaoin'[#id], 'shrdlu'[#id], permission-to(V3:'shrdlu'[#id], verb.follow('shrdlu'[#id], 'player'[#id])))", this.game.ontology);
        if (pattern1.subsumes(action, true, new Bindings()))
            return true;
        if (pattern2.subsumes(action, true, new Bindings()))
            return true;
        if (pattern3.subsumes(action, true, new Bindings()))
            return true;
        if (pattern4.subsumes(action, true, new Bindings()))
            return true;
        if (pattern5.subsumes(action, true, new Bindings()))
            return true;
        if (pattern6.subsumes(action, true, new Bindings()))
            return true;
        return false;
    };
    ShrdluGameScript.prototype.predicateIsAboutTakingShrdlu = function (predicate) {
        var pattern1 = Term.fromString("verb.can('shrdlu'[#id], verb.help('shrdlu'[#id],'player'[#id], 'distress-signal1'[#id]))", this.game.ontology);
        var pattern2 = Term.fromString("verb.can('shrdlu'[#id], verb.help('shrdlu'[#id],'player'[#id], 'distress-signal2'[#id]))", this.game.ontology);
        if (pattern1.subsumes(predicate, true, new Bindings()))
            return true;
        if (pattern2.subsumes(predicate, true, new Bindings()))
            return true;
        return false;
    };
    ShrdluGameScript.prototype.playerAskedToRepairTheDatapad = function () {
        if (this.contextEtaoin == null ||
            this.contextQwerty == null ||
            this.contextShrdlu == null) {
            this.contextEtaoin = this.game.etaoinAI.contextForSpeaker(this.playerID);
            this.contextQwerty = this.game.qwertyAI.contextForSpeaker(this.playerID);
            this.contextShrdlu = this.game.shrdluAI.contextForSpeaker(this.playerID);
        }
        for (var _i = 0, _a = [this.contextQwerty, this.contextEtaoin, this.contextShrdlu]; _i < _a.length; _i++) {
            var context = _a[_i];
            if (context != null) {
                var p1 = context.lastPerformativeBy(this.playerID);
                if (p1 != null && p1.performative != null && p1.timeStamp == this.game.in_game_seconds - 1) {
                    var pattern1 = Term.fromString("verb.repair(X:[#id], 'shuttle-datapad'[#id])", this.game.ontology);
                    var perf = p1.performative;
                    if (perf.functor.is_a(this.game.ontology.getSort("perf.q.action")) ||
                        perf.functor.is_a(this.game.ontology.getSort("perf.request.action"))) {
                        var action = (perf.attributes[1]).term;
                        if (pattern1.subsumes(action, true, new Bindings()))
                            return perf;
                    }
                }
            }
        }
        return null;
    };
    ShrdluGameScript.prototype.setupShrdluSelfRepairGoals = function () {
        this.game.shrdluAI.goals = [];
        this.game.shrdluAI.currentGoal = null;
        var o = this.game.shrdluAI.o;
        var time = this.game.shrdluAI.timeStamp;
        var goal = new AIGoal(Term.fromString("verb.repair('shrdlu'[#id], 'shrdlu'[#id])", o), [Term.fromString("verb.go-to('shrdlu'[#id],'" + 98 * 8 + "'[number],'" + 9 * 8 + "'[number],'Aurora Station'[symbol])", o),
            Term.fromString("verb.go-to('shrdlu'[#id],'" + 81 * 8 + "'[number],'" + 7 * 8 + "'[number],'Aurora Station'[symbol])", o),
            Term.fromString("action.stay('shrdlu'[#id],'" + 81 * 8 + "'[number],'" + 7 * 8 + "'[number],'Aurora Station'[symbol],'" + 8 * 60 * 60 + "'[number])", o)], 100, time, -1);
        this.game.shrdluAI.goals.push(goal);
        goal = new AIGoal(Term.fromString("verb.repair('shrdlu'[#id], 'broken-stasis-pod'[#id])", o), [Term.fromString("verb.go-to('shrdlu'[#id],'" + 5 * 8 + "'[number],'" + 26 * 8 + "'[number],'Aurora Station'[symbol])", o),
            Term.fromString("verb.go-to('shrdlu'[#id],'" + 7 * 8 + "'[number],'" + 9 * 8 + "'[number],'Aurora Station'[symbol])", o),
            Term.fromString("verb.go-to('shrdlu'[#id],'" + 7 * 8 + "'[number],'" + 8 * 8 + "'[number],'Aurora Station'[symbol])", o),
            Term.fromString("action.stay('shrdlu'[#id],'" + 7 * 8 + "'[number],'" + 8 * 8 + "'[number],'Aurora Station'[symbol],'" + 1 * 60 * 60 + "'[number])", o)], 90, time, -1);
        this.game.shrdluAI.goals.push(goal);
        goal = new AIGoal(Term.fromString("verb.repair('shrdlu'[#id], 'etaoin'[#id])", o), [Term.fromString("verb.go-to('shrdlu'[#id],'" + 87 * 8 + "'[number],'" + 35 * 8 + "'[number],'Aurora Station'[symbol])", o),
            Term.fromString("verb.go-to('shrdlu'[#id],'" + 91 * 8 + "'[number],'" + 22 * 8 + "'[number],'Aurora Station'[symbol])", o),
            Term.fromString("verb.go-to('shrdlu'[#id],'" + 7 * 8 + "'[number],'" + 8 * 8 + "'[number],'Aurora Station'[symbol])", o),
            Term.fromString("action.stay('shrdlu'[#id],'" + 7 * 8 + "'[number],'" + 8 * 8 + "'[number],'Aurora Station'[symbol],'" + 1 * 60 * 60 + "'[number])", o)], 80, time, -1);
        this.game.shrdluAI.goals.push(goal);
    };
    ShrdluGameScript.prototype.setupShrdluRepairCommsGoals = function () {
        // when this function is called, SHRDLU has already repaired the stasis pod:
        for (var _i = 0, _a = [this.game.etaoinAI, this.game.qwertyAI, this.game.shrdluAI]; _i < _a.length; _i++) {
            var ai = _a[_i];
            var se = ai.longTermMemory.findSentenceEntry(Sentence.fromString("property.broken('broken-stasis-pod'[#id])", this.game.ontology));
            if (se != null)
                se.sentence.sign[0] = false;
        }
        this.game.shrdluAI.goals = [];
        this.game.shrdluAI.currentGoal = null;
        var o = this.game.shrdluAI.o;
        var time = this.game.shrdluAI.timeStamp;
        var goal = new AIGoal(Term.fromString("verb.repair('shrdlu'[#id], 'comm-tower'[#id])", o), [Term.fromString("verb.go-to('shrdlu'[#id],'" + 6 * 8 + "'[number],'" + 23 * 8 + "'[number],'Aurora Station Outdoors'[symbol])", o),
            Term.fromString("action.stay('shrdlu'[#id],'" + 6 * 8 + "'[number],'" + 23 * 8 + "'[number],'Aurora Station Outdoors'[symbol],'" + 1 * 30 * 60 + "'[number])", o)], 100, time, -1);
        this.game.shrdluAI.goals.push(goal);
    };
    ShrdluGameScript.prototype.setupShrdluAgendaGoals = function () {
        this.game.shrdluAI.goals = [];
        this.game.shrdluAI.currentGoal = null;
        var o = this.game.shrdluAI.o;
        var time = this.game.shrdluAI.timeStamp;
        // default goal of going back:
        var goal = new AIGoal(Term.fromString("verb.go-to('shrdlu'[#id],'location-as25'[#id])", o), [Term.fromString("verb.go-to('shrdlu'[#id],'" + 81 * 8 + "'[number],'" + 7 * 8 + "'[number],'Aurora Station'[symbol])", o)], 0, time, 10 * 60);
        this.game.shrdluAI.goals.push(goal);
        // Power plant
        goal = new AIGoal(Term.fromString("verb.repair('shrdlu'[#id], 'aurora-station'[#id])", o), [Term.fromString("verb.go-to('shrdlu'[#id],'" + 72 * 8 + "'[number],'" + 46 * 8 + "'[number],'Aurora Station Outdoors'[symbol])", o),
            Term.fromString("action.stay('shrdlu'[#id],'" + 72 * 8 + "'[number],'" + 46 * 8 + "'[number],'Aurora Station Outdoors'[symbol],'" + 1 * 30 * 60 + "'[number])", o)], 100, time, 8 * 60 * 60);
        this.game.shrdluAI.goals.push(goal);
        // oxygen tanks
        goal = new AIGoal(Term.fromString("verb.repair('shrdlu'[#id], 'aurora-station'[#id])", o), [Term.fromString("verb.go-to('shrdlu'[#id],'" + 41 * 8 + "'[number],'" + 19 * 8 + "'[number],'Aurora Station Outdoors'[symbol])", o),
            Term.fromString("action.stay('shrdlu'[#id],'" + 41 * 8 + "'[number],'" + 19 * 8 + "'[number],'Aurora Station Outdoors'[symbol],'" + 1 * 30 * 60 + "'[number])", o)], 100, time + 1 * 60 * 60, 8 * 60 * 60);
        this.game.shrdluAI.goals.push(goal);
        // water tank
        goal = new AIGoal(Term.fromString("verb.repair('shrdlu'[#id], 'aurora-station'[#id])", o), [Term.fromString("verb.go-to('shrdlu'[#id],'" + 58 * 8 + "'[number],'" + 18 * 8 + "'[number],'Aurora Station Outdoors'[symbol])", o),
            Term.fromString("action.stay('shrdlu'[#id],'" + 58 * 8 + "'[number],'" + 18 * 8 + "'[number],'Aurora Station Outdoors'[symbol],'" + 1 * 30 * 60 + "'[number])", o)], 100, time + 2 * 60 * 60, 8 * 60 * 60);
        this.game.shrdluAI.goals.push(goal);
        // green houses
        goal = new AIGoal(Term.fromString("verb.repair('shrdlu'[#id], 'aurora-station'[#id])", o), [Term.fromString("verb.go-to('shrdlu'[#id],'" + 52 * 8 + "'[number],'" + 12 * 8 + "'[number],'Aurora Station Outdoors'[symbol])", o),
            Term.fromString("action.stay('shrdlu'[#id],'" + 52 * 8 + "'[number],'" + 12 * 8 + "'[number],'Aurora Station Outdoors'[symbol],'" + 1 * 30 * 60 + "'[number])", o)], 100, time + 3 * 60 * 60, 8 * 60 * 60);
        this.game.shrdluAI.goals.push(goal);
        // recycling
        goal = new AIGoal(Term.fromString("verb.repair('shrdlu'[#id], 'aurora-station'[#id])", o), [Term.fromString("verb.go-to('shrdlu'[#id],'" + 32 * 8 + "'[number],'" + 19 * 8 + "'[number],'Aurora Station Outdoors'[symbol])", o),
            Term.fromString("action.stay('shrdlu'[#id],'" + 32 * 8 + "'[number],'" + 19 * 8 + "'[number],'Aurora Station Outdoors'[symbol],'" + 1 * 30 * 60 + "'[number])", o)], 100, time + 4 * 60 * 60, 8 * 60 * 60);
        this.game.shrdluAI.goals.push(goal);
    };
    ShrdluGameScript.prototype.setupQwertyAgenda = function () {
        this.game.qwertyAI.goals = [];
        this.game.qwertyAI.currentGoal = null;
        var o = this.game.qwertyAI.o;
        var time = this.game.qwertyAI.timeStamp;
        var goal = new AIGoal(Term.fromString("verb.analyze('qwerty'[#id],'infirmary-console'[#id])", o), [Term.fromString("verb.go-to('qwerty'[#id],'" + 9 * 8 + "'[number],'" + 27 * 8 + "'[number],'Aurora Station'[symbol])", o),
            Term.fromString("action.stay('qwerty'[#id],'" + 9 * 8 + "'[number],'" + 27 * 8 + "'[number],'Aurora Station'[symbol],'" + 30 * 60 + "'[number])", o)], 100, time, 8 * 60 * 60);
        this.game.qwertyAI.goals.push(goal);
        goal = new AIGoal(Term.fromString("verb.fill('qwerty'[#id])", o), [Term.fromString("verb.go-to('qwerty'[#id],'" + 3 * 8 + "'[number],'" + 27 * 8 + "'[number],'Aurora Station'[symbol])", o),
            Term.fromString("action.stay('qwerty'[#id],'" + 3 * 8 + "'[number],'" + 27 * 8 + "'[number],'Aurora Station'[symbol],'" + 30 * 60 + "'[number])", o)], 100, time + 2 * 60 * 60, 8 * 60 * 60);
        this.game.qwertyAI.goals.push(goal);
        goal = new AIGoal(Term.fromString("verb.analyze('qwerty'[#id],'infirmary-fridge'[#id])", o), [Term.fromString("verb.go-to('qwerty'[#id],'" + 10 * 8 + "'[number],'" + 18 * 8 + "'[number],'Aurora Station'[symbol])", o),
            Term.fromString("action.stay('qwerty'[#id],'" + 10 * 8 + "'[number],'" + 18 * 8 + "'[number],'Aurora Station'[symbol],'" + 30 * 60 + "'[number])", o)], 100, time + 4 * 60 * 60, 8 * 60 * 60);
        this.game.qwertyAI.goals.push(goal);
        goal = new AIGoal(Term.fromString("verb.analyze('qwerty'[#id],'location-as22'[#id])", o), [Term.fromString("verb.go-to('qwerty'[#id],'" + 34 * 8 + "'[number],'" + 14 * 8 + "'[number],'Aurora Station'[symbol])", o),
            Term.fromString("action.stay('qwerty'[#id],'" + 34 * 8 + "'[number],'" + 14 * 8 + "'[number],'Aurora Station'[symbol],'" + 30 * 60 + "'[number])", o)], 100, time + 6 * 60 * 60, 8 * 60 * 60);
        this.game.qwertyAI.goals.push(goal);
        // default goal of going back:
        goal = new AIGoal(Term.fromString("verb.go-to('qwerty'[#id],'location-as25'[#id])", o), [Term.fromString("verb.go-to('qwerty'[#id],'" + 8 * 8 + "'[number],'" + 29 * 8 + "'[number],'Aurora Station'[symbol])", o)], 0, time, 10 * 60);
        this.game.qwertyAI.goals.push(goal);
    };
    ShrdluGameScript.prototype.playerHasItemP = function (itemId) {
        for (var _i = 0, _a = this.game.currentPlayer.inventory; _i < _a.length; _i++) {
            var item = _a[_i];
            if (item.ID == itemId)
                return true;
        }
        return false;
    };
    ShrdluGameScript.prototype.qwertyIntention = function (pattern) {
        pattern = pattern.split("$QWERTY").join("'" + this.qwertyID + "'[#id]");
        pattern = pattern.split("$ETAOIN").join("'" + this.etaoinID + "'[#id]");
        pattern = pattern.split("$PLAYER").join("'" + this.playerID + "'[#id]");
        var term = Term.fromString(pattern, this.game.ontology);
        if (term == null) {
            console.error("qwertyIntention: cannot parse pattern " + pattern);
        }
        else {
            this.game.qwertyAI.queueIntention(term, null, null);
        }
    };
    ShrdluGameScript.prototype.qwertyMoves = function (x, y, map) {
        this.game.qwertyAI.clearEpisodeTerms();
        var q = new A4ScriptExecutionQueue(this.game.qwertyAI.robot, this.game.qwertyAI.robot.map, this.game, null);
        var s = new A4Script(A4_SCRIPT_GOTO_OPENING_DOORS, map.name, null, 0, false, false);
        s.x = x;
        s.y = y;
        q.scripts.push(s);
        this.game.qwertyAI.robot.addScriptQueue(q);
    };
    ShrdluGameScript.prototype.qwertyMovesOverrideable = function (x, y, map, action) {
        this.game.qwertyAI.clearEpisodeTerms();
        var q = new A4ScriptExecutionQueue(this.game.qwertyAI.robot, this.game.qwertyAI.robot.map, this.game, null);
        var s = new A4Script(A4_SCRIPT_GOTO_OPENING_DOORS, map.name, null, 0, false, false);
        s.x = x;
        s.y = y;
        q.scripts.push(s);
        this.game.qwertyAI.setNewAction(action, null, q, null);
    };
    ShrdluGameScript.prototype.shrdluMoves = function (x, y, map) {
        this.game.shrdluAI.clearEpisodeTerms();
        var q = new A4ScriptExecutionQueue(this.game.shrdluAI.robot, this.game.shrdluAI.robot.map, this.game, null);
        var s = new A4Script(A4_SCRIPT_GOTO_OPENING_DOORS, map.name, null, 0, false, false);
        s.x = x;
        s.y = y;
        q.scripts.push(s);
        this.game.shrdluAI.clearCurrentAction();
        this.game.shrdluAI.robot.addScriptQueue(q);
    };
    ShrdluGameScript.prototype.shrdluMovesOverrideable = function (x, y, map, action) {
        this.game.shrdluAI.clearEpisodeTerms();
        var q = new A4ScriptExecutionQueue(this.game.shrdluAI.robot, this.game.shrdluAI.robot.map, this.game, null);
        var s = new A4Script(A4_SCRIPT_GOTO_OPENING_DOORS, map.name, null, 0, false, false);
        s.x = x;
        s.y = y;
        q.scripts.push(s);
        this.game.shrdluAI.setNewAction(action, null, q, null);
    };
    ShrdluGameScript.prototype.qwertyDropsObject = function (objectID) {
        var q = new A4ScriptExecutionQueue(this.game.qwertyAI.robot, this.game.qwertyAI.robot.map, this.game, null);
        var s = new A4Script(A4_SCRIPT_DROP, objectID, null, 0, false, false);
        q.scripts.push(s);
        this.game.qwertyAI.robot.addScriptQueue(q);
    };
    ShrdluGameScript.prototype.etaoinSays = function (pattern) {
        pattern = pattern.split("$QWERTY").join("'" + this.qwertyID + "'[#id]");
        pattern = pattern.split("$ETAOIN").join("'" + this.etaoinID + "'[#id]");
        pattern = pattern.split("$PLAYER").join("'" + this.playerID + "'[#id]");
        var term = Term.fromString(pattern, this.game.ontology);
        if (term == null) {
            console.error("etaoinSays: cannot parse pattern " + pattern);
        }
        else {
            var term2 = new Term(this.game.ontology.getSort("action.talk"), [new ConstantTermAttribute(this.playerID, this.game.ontology.getSort("#id")),
                new TermTermAttribute(term)]);
            this.game.etaoinAI.queueIntention(term2, null, null);
        }
    };
    ShrdluGameScript.prototype.shrdluSays = function (pattern) {
        pattern = pattern.split("$QWERTY").join("'" + this.qwertyID + "'[#id]");
        pattern = pattern.split("$ETAOIN").join("'" + this.etaoinID + "'[#id]");
        pattern = pattern.split("$PLAYER").join("'" + this.playerID + "'[#id]");
        var term = Term.fromString(pattern, this.game.ontology);
        if (term == null) {
            console.error("etaoinSays: cannot parse pattern " + pattern);
        }
        else {
            var term2 = new Term(this.game.ontology.getSort("action.talk"), [new ConstantTermAttribute(this.playerID, this.game.ontology.getSort("#id")),
                new TermTermAttribute(term)]);
            this.game.shrdluAI.queueIntention(term2, null, null);
        }
    };
    ShrdluGameScript.prototype.queueThoughtBubble = function (message) {
        this.thoughtBubbleQueue.push(message);
    };
    ShrdluGameScript.prototype.processQueuedThoughtBubbles = function () {
        if (this.thoughtBubbleQueue.length == 0)
            return;
        if (this.game.currentPlayer.isIdle() &&
            !this.game.qwertyAI.robot.isTalking() &&
            !this.game.shrdluAI.robot.isTalking() &&
            this.game.etaoinAI.isIdle()) {
            this.game.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, this.thoughtBubbleQueue[0], A4_DIRECTION_NONE, this.game);
            this.thoughtBubbleQueue.splice(0, 1);
        }
    };
    ShrdluGameScript.prototype.saveToXML = function () {
        var xmlString = "";
        xmlString += "<ShrdluGameScript\n";
        // Intro:
        xmlString += "act=\"" + this.act + "\"\n";
        xmlString += "act_intro_state=\"" + this.act_intro_state + "\"\n";
        xmlString += "act_intro_state_timer=\"" + this.act_intro_state_timer + "\"\n";
        xmlString += "act_intro_state_start_time=\"" + this.act_intro_state_start_time + "\"\n";
        xmlString += "act_intro_chair_x=\"" + this.act_intro_chair_x + "\"\n";
        xmlString += "act_intro_chair_y=\"" + this.act_intro_chair_y + "\"\n";
        // act 1:
        xmlString += "act_1_state=\"" + this.act_1_state + "\"\n";
        xmlString += "act_1_state_timer=\"" + this.act_1_state_timer + "\"\n";
        xmlString += "act_1_state_start_time=\"" + this.act_1_state_start_time + "\"\n";
        xmlString += "act_1_number_of_useless_etaoin_answers=\"" + this.act_1_number_of_useless_etaoin_answers + "\"\n";
        xmlString += "act_1_know_etaoin_is_an_AI=\"" + this.act_1_know_etaoin_is_an_AI + "\"\n";
        xmlString += "act_1_player_gone_outside=\"" + this.act_1_player_gone_outside + "\"\n";
        xmlString += "act_1_asked_about_being_alone_to_etaoin=\"" + this.act_1_asked_about_being_alone_to_etaoin + "\"\n";
        xmlString += "act_1_asked_about_being_alone_to_qwerty=\"" + this.act_1_asked_about_being_alone_to_qwerty + "\"\n";
        xmlString += "act_1_stasis_thread_state=\"" + this.act_1_stasis_thread_state + "\"\n";
        xmlString += "act_1_stasis_thread_state_timer=\"" + this.act_1_stasis_thread_state_timer + "\"\n";
        xmlString += "act_1_asked_about_tools=\"" + this.act_1_asked_about_tools + "\"\n";
        xmlString += "act_1_asked_about_battery=\"" + this.act_1_asked_about_battery + "\"\n";
        xmlString += "act_1_stated_spacesuit_is_broken=\"" + this.act_1_stated_spacesuit_is_broken + "\"\n";
        xmlString += "act_1_asked_about_bruce_alper=\"" + this.act_1_asked_about_bruce_alper[0] + "," +
            this.act_1_asked_about_bruce_alper[1] + "," +
            this.act_1_asked_about_bruce_alper[2] + "\"\n";
        xmlString += "act_1_asked_about_corpse=\"" + this.act_1_asked_about_corpse[0] + "," +
            this.act_1_asked_about_corpse[1] + "," +
            this.act_1_asked_about_corpse[2] + "\"\n";
        xmlString += "act_2_state=\"" + this.act_2_state + "\"\n";
        xmlString += "act_2_state_timer=\"" + this.act_2_state_timer + "\"\n";
        xmlString += "act_2_state_start_time=\"" + this.act_2_state_start_time + "\"\n";
        xmlString += "act_2_datapad_state=\"" + this.act_2_datapad_state + "\"\n";
        xmlString += "act_2_datapad_state_timer=\"" + this.act_2_datapad_state_timer + "\"\n";
        xmlString += "act_2_repair_shuttle_state=\"" + this.act_2_repair_shuttle_state + "\"\n";
        xmlString += "act_2_repair_shuttle_state_timer=\"" + this.act_2_repair_shuttle_state_timer + "\"\n";
        xmlString += "act_3_state=\"" + this.act_3_state + "\"\n";
        xmlString += "act_3_state_timer=\"" + this.act_3_state_timer + "\"\n";
        xmlString += "act_3_state_start_time=\"" + this.act_3_state_start_time + "\"\n";
        xmlString += "act_3_repair_tardis_console_state=\"" + this.act_3_repair_tardis_console_state + "\"\n";
        xmlString += "act_3_repair_tardis_console_state_timer=\"" + this.act_3_repair_tardis_console_state_timer + "\"\n";
        xmlString += "act_3_repair_tardis_cable_state=\"" + this.act_3_repair_tardis_cable_state + "\"\n";
        xmlString += "act_3_repair_tardis_cable_state_timer=\"" + this.act_3_repair_tardis_cable_state_timer + "\"\n";
        xmlString += "finding_life_side_plot_taken_question=\"" + this.finding_life_side_plot_taken_question + "\"\n";
        xmlString += "finding_life_side_plot_analyzed_question=\"" + this.finding_life_side_plot_analyzed_question + "\"\n";
        xmlString += "what_is_dust_side_plot_taken_question=\"" + this.what_is_dust_side_plot_taken_question + "\"\n";
        xmlString += "player_has_asked_to_take_shrdlu=\"" + this.player_has_asked_to_take_shrdlu + "\"\n";
        xmlString += "/>\n";
        for (var tmp in this.thoughtBubbleQueue) {
            xmlString += "<thoughtBubbleQueue value=\"" + tmp + "\"/>\n";
        }
        for (var tmp in this.act_1_explored_rooms) {
            xmlString += "<act_1_explored_rooms value=\"" + tmp + "\"/>\n";
        }
        return xmlString;
    };
    ShrdluGameScript.prototype.restoreFromXML = function (xml) {
        this.act = xml.getAttribute("act");
        this.act_intro_state = Number(xml.getAttribute("act_intro_state"));
        this.act_intro_state_timer = Number(xml.getAttribute("act_intro_state_timer"));
        this.act_intro_state_start_time = Number(xml.getAttribute("act_intro_state_start_time"));
        this.act_intro_chair_x = Number(xml.getAttribute("act_intro_chair_x"));
        this.act_intro_chair_y = Number(xml.getAttribute("act_intro_chair_y"));
        this.act_1_state = Number(xml.getAttribute("act_1_state"));
        this.act_1_state_timer = Number(xml.getAttribute("act_1_state_timer"));
        this.act_1_state_start_time = Number(xml.getAttribute("act_1_state_start_time"));
        this.act_1_number_of_useless_etaoin_answers = Number(xml.getAttribute("act_1_number_of_useless_etaoin_answers"));
        this.act_1_explored_rooms = [];
        for (var _i = 0, _a = getElementChildrenByTag(xml, "act_1_explored_rooms"); _i < _a.length; _i++) {
            var xml_tmp = _a[_i];
            this.act_1_explored_rooms.push(xml_tmp.getAttribute("value"));
        }
        this.act_1_know_etaoin_is_an_AI = xml.getAttribute("act_1_know_etaoin_is_an_AI") == "true";
        this.act_1_player_gone_outside = xml.getAttribute("act_1_player_gone_outside") == "true";
        this.act_1_asked_about_being_alone_to_etaoin = xml.getAttribute("act_1_asked_about_being_alone_to_etaoin") == "true";
        this.act_1_asked_about_being_alone_to_qwerty = xml.getAttribute("act_1_asked_about_being_alone_to_qwerty") == "true";
        this.act_1_stasis_thread_state = Number(xml.getAttribute("act_1_stasis_thread_state"));
        this.act_1_stasis_thread_state_timer = Number(xml.getAttribute("act_1_stasis_thread_state_timer"));
        this.act_1_asked_about_tools = xml.getAttribute("act_1_asked_about_tools") == "true";
        this.act_1_asked_about_battery = xml.getAttribute("act_1_asked_about_battery") == "true";
        this.act_1_stated_spacesuit_is_broken = xml.getAttribute("act_1_stated_spacesuit_is_broken") == "true";
        this.act_1_asked_about_bruce_alper = [];
        var tmp1 = xml.getAttribute("act_1_asked_about_bruce_alper").split(",");
        for (var _b = 0, tmp1_1 = tmp1; _b < tmp1_1.length; _b++) {
            var tmp2 = tmp1_1[_b];
            this.act_1_asked_about_bruce_alper.push(tmp2 == "true");
        }
        this.act_1_asked_about_corpse = [];
        tmp1 = xml.getAttribute("act_1_asked_about_corpse").split(",");
        for (var _c = 0, tmp1_2 = tmp1; _c < tmp1_2.length; _c++) {
            var tmp2 = tmp1_2[_c];
            this.act_1_asked_about_corpse.push(tmp2 == "true");
        }
        this.act_2_state = Number(xml.getAttribute("act_2_state"));
        this.act_2_state_timer = Number(xml.getAttribute("act_2_state_timer"));
        this.act_2_state_start_time = Number(xml.getAttribute("act_2_state_start_time"));
        this.act_2_datapad_state = Number(xml.getAttribute("act_2_datapad_state"));
        this.act_2_datapad_state_timer = Number(xml.getAttribute("act_2_datapad_state_timer"));
        this.act_2_repair_shuttle_state = Number(xml.getAttribute("act_2_repair_shuttle_state"));
        this.act_2_repair_shuttle_state_timer = Number(xml.getAttribute("act_2_repair_shuttle_state_timer"));
        this.act_3_state = Number(xml.getAttribute("act_3_state"));
        this.act_3_state_timer = Number(xml.getAttribute("act_3_state_timer"));
        this.act_3_state_start_time = Number(xml.getAttribute("act_3_state_start_time"));
        this.act_3_repair_tardis_console_state = Number(xml.getAttribute("act_3_repair_tardis_console_state"));
        this.act_3_repair_tardis_console_state_timer = Number(xml.getAttribute("act_3_repair_tardis_console_state_timer"));
        this.act_3_repair_tardis_cable_state = Number(xml.getAttribute("act_3_repair_tardis_cable_state"));
        this.act_3_repair_tardis_cable_state_timer = Number(xml.getAttribute("act_3_repair_tardis_cable_state_timer"));
        this.finding_life_side_plot_taken_question = xml.getAttribute("finding_life_side_plot_taken_question") == "true";
        this.finding_life_side_plot_analyzed_question = xml.getAttribute("finding_life_side_plot_analyzed_question") == "true";
        this.what_is_dust_side_plot_taken_question = xml.getAttribute("what_is_dust_side_plot_taken_question") == "true";
        this.player_has_asked_to_take_shrdlu = xml.getAttribute("player_has_asked_to_take_shrdlu") == "true";
        this.thoughtBubbleQueue = [];
        for (var _d = 0, _e = getElementChildrenByTag(xml, "thoughtBubbleQueue"); _d < _e.length; _d++) {
            var xml_tmp = _e[_d];
            this.thoughtBubbleQueue.push(xml_tmp.getAttribute("value"));
        }
        this.contextEtaoin = this.game.etaoinAI.contextForSpeaker(this.playerID);
        this.contextQwerty = this.game.qwertyAI.contextForSpeaker(this.playerID);
        this.contextShrdlu = this.game.shrdluAI.contextForSpeaker(this.playerID);
        this.contextPlayer = this.game.shrdluAI.contextForSpeaker(this.etaoinID);
    };
    return ShrdluGameScript;
}());
