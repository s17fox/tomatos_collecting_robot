# tomatos_collecting_robot
Η εργασία μας αποτελείται από δύο ρομποτικά συστήματα. Τη μηχανή περισυλλογής που είναι ένα ξεχωριστό ρομπότ (μετατροπή ενός εκτυπωτή inkjet) κατασκευασμένο ειδικά για την περίπτωση χρησιμοποιώντας Arduino Mega 1280. Διαθέτει πληθώρα γραμμών εισόδου-εξόδου, καθώς και δυνατότητες χρήσης των PWM. Το ρομπότ προχωρά σε 4 ρόδες για μεγαλύτερη σταθερότητα και κατευθύνεται με ψηφιακή πυξίδα. Επικοινωνεί ασύρματα με Bluetooth. Και το τμήμα παραλαβής και εξόδου που υλοποιείται με LEGO EV3.

Πλεονεκτήματα
	Κινείται σε κάθε έδαφος διαγράφοντας διάφορες τροχιές.
	Αναφέρει ασύρματα κάθε φάση της λειτουργίας του.
	Λαμβάνει και εκτελεί εντολές.
	Δεν χρειάζεται ειδικές υποδομές.
	Συνεργάζεται με άλλα υπολογιστικά συστήματα.

Βασικές καινοτομίες της εργασίας μας:
•	Ο τρόπος με τον οποίο συλλέγουμε τις ντομάτες. Χρησιμοποιούμε έναν κινητήρα για να πραγματοποιήσουμε τις δύο κινήσεις που κάνει το ανθρώπινο χέρι. Πιάνει τον ώριμο καρπό, τον γυρίζει για να κόψει το κοτσάνι και στη συνέχεια τον τοποθετεί στο τελάρο. 
•	Χρησιμοποιήσαμε RGB αισθητήρα χρώματος 16 pixels για να έχουμε δυνατότητα επιλογής χρώματος ώριμου καρπού. 
•	Σαρώσαμε την περιοχή με αισθητήρα κόκκινου χρώματος και laser. Αυξήσαμε την ευαισθησία χρησιμοποιώντας έναν μαύρο σωλήνα και τον τηλεφακό από μια φωτογραφική μηχανή μιας χρήσης. 

Υλοποίηση:   
•	Χρησιμοποιήσαμε BASCOM Compiler.
•	Τοποθετήσαμε βηματικούς κινητήρες για πολύ μεγάλη ακρίβεια θέσης. 
•	Ο συλλέκτης κινείται σε αμαξίδιο τεσσάρων τροχών.
•	Ο βραχίονας κινείται κατακόρυφα, περιστροφικά και οριζόντια.
•	Τροφοδοτήσαμε με ρεύμα από το δίκτυο αλλά σε πραγματικό επίπεδο θα εκμεταλλεύεται την ηλεκτρική ενέργεια που θα παράγεται με τη γεωθερμία για να λειτουργήσει κατά τη διάρκεια μέρας και νύχτας. Το πλεόνασμα της ενέργειας θα  αποθηκεύεται  σε μπαταρίες για χρήση από άλλες συσκευές. 
•	Με κάμερες στο χώρο θα μπορούμε να εντοπίζουμε με ακρίβεια τη θέση των καρπών και να τους συλλέγουμε με εντελώς τυφλά ρομπότ.

ΙΣΤΟΡΙΚΟ:

Το έργο της ομάδας Cypher Group ξεκίνησε να μελετάται από τον Νοέμβριο του 2017 προκειμένου να συμμετάσχει στο διαγωνισμό WRO του 2018. Η πρώτη έκδοσή του υλοποιήθηκε στις αρχές του 2018. 
Ακολούθησαν αλλαγές και βελτιώσεις μέχρι το καλοκαίρι του 2018 όπου παρουσιάστηκε στον διαγωνισμό WRO FOOD MATTERS. Για λόγους που δεν μάθαμε ποτέ δεν διακρίθηκε.
Συμμετείχε στον διαγωνισμό SMART CITY  του ιδρύματος Σταύρος Νιάρχος τον Ιούλιο 2018 όπου κατέλαβε την τέταρτη θέση.
‘Έκτοτε παρουσιάσθηκε: 
•	Σε ηλεκτρονική μορφή στο 4ο Διεθνές Συνέδριο για την Προώθηση της Εκπαιδευτικής Καινοτομίας στη Λάρισα τον Οκτώβριο του 2018 
•	Και δημοσιεύθηκε στο περιοδικό i-Teacher Δεκέμβριος 2018.
Πλήρη στοιχεία για την προβολή του έργου θα βρείτε στο site
https://s17fox.wixsite.com/food/eellak
που για την περίπτωση κατασκευάσαμε.
Το κόστος κατασκευής ήταν ελάχιστο. Γύρω στα 100 ευρώ γιατί αγοράσαμε μόνο το αμαξίδιο, το arduino mega, το Bluetooth, την πυξίδα, τους ενισχυτές για τα μοτεράκια και τα εξαρτήματα για το τροφοδοτικό. Το EV3 το είχαμε αγοράσει για να πειραματιστούνε τα παιδιά παλαιότερα. Όλα τα εξαρτήματα είναι αγορασμένα από το ebay επομένως δεν έχουμε καμία οικονομικά απαίτηση από τη διοργάνωση του διαγωνισμού.
Σήμερα οι δύο μαθητές φοιτούν στην Γ’ Λυκείου, η μαθήτρια στη Β’ Λυκείου και καταλαβαίνετε ότι ο φόρτος των μαθημάτων είναι υπερβολικός. Επίσης το έργο μεταφέρεται δύσκολα και είναι επιρρεπές σε ζημιές λόγω των ευαίσθητων εξαρτημάτων του.
Θα θέλαμε να σας ενημερώσουμε ότι στην τελική παρουσίαση, εάν αυτή υπάρξει, θα είμαστε σε θέση να παρουσιάσουμε το έργο σε ηλεκτρονική μορφή (video & power point). Θα βρείτε όλο το υλικό στο παραπάνω site.
Σας ευχαριστούμε πολύ.
Η ομάδα Cypher Group.
