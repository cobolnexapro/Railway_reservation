# Indian Railway Reservation System (IRRS) - IBM i

This repository contains a comprehensive case study of a railway reservation system built entirely for the IBM i (AS/400) ecosystem. The application utilizes ILE COBOL for its core business logic, Native Record Level Access (RLA) for database operations, and Data Description Specifications (DDS) for both physical database tables and the interactive 5250 green-screen user interface.

## System Features

* **User Management & Authentication:** Secure sign-on and registration module. Differentiates between standard passengers and administrators.
* **Train & Schedule Enquiry:** Allows users to search for trains between source and destination stations, viewing departure/arrival times, total capacity, and real-time seat availability.
* **Complex Booking Engine:** * Auto-generates unique PNRs for successful bookings.
  * Handles dynamic berth types and quota allocations (General, Ladies, Tatkal).
  * Calculates fares based on passenger demographics (e.g., age) and selected quotas.
* **Payments & Cancellations:** Integrates a simulated payment ledger. Supports ticket cancellations with dynamic, tiered refund calculations and frees up seat availability instantly.
* **Administrative Console:** Allows authorized staff to maintain train schedules, manage quotas, and generate high-level Booking & Revenue reports (tracking active vs. cancelled bookings, total revenue, and refunds paid).

## Architecture & Module Breakdown

### Core Application Program (COBOL)
* **`RAILMAIN.CBL`**: The monolithic driver program for the entire application. It controls screen navigation, processes user inputs, performs heavy business logic (fare calculation, quota decrements), and manages all DB2 file I/O operations.

### Database Definitions (DDS Physical Files)
* **`TRAINPF.PF`**: Train Master file. Stores schedule details, route codes, and partitioned seat availability across different quotas.
* **`USERPF.PF`**: User/Customer Master file. Stores authentication credentials, demographics, and contact information.
* **`BOOKPF.PF`**: Booking Ledger. The core transactional table tracking PNRs, passenger details, assigned berths, and current booking status (Active/Cancelled).
* **`PAYPF.PF`**: Payment Transaction Ledger. Records financial transactions, simulated gateway reference numbers, payment modes, and amounts.

### User Interface (DDS Display File)
* **`RAILDSPF.DSPF`**: Contains 13 distinct 5250 screen formats serving the interactive frontend. Screens include the Login portal, Main Menu, Train Search, Booking Data Entry, and the Administrative Revenue Dashboard.

## Compilation & Deployment

To deploy this application on an IBM i system, the source code must be compiled in the correct dependency order:

1.  **Compile Database Files:**
    ```bash
    CRTPF FILE(yourlib/TRAINPF) SRCFILE(yourlib/QDDSSRC)
    CRTPF FILE(yourlib/USERPF) SRCFILE(yourlib/QDDSSRC)
    CRTPF FILE(yourlib/BOOKPF) SRCFILE(yourlib/QDDSSRC)
    CRTPF FILE(yourlib/PAYPF) SRCFILE(yourlib/QDDSSRC)
    ```
2.  **Compile Display File:**
    ```bash
    CRTDSPF FILE(yourlib/RAILDSPF) SRCFILE(yourlib/QDDSSRC)
    ```
3.  **Compile COBOL Program:**
    ```bash
    CRTBNDCBL PGM(yourlib/RAILMAIN) SRCFILE(yourlib/QCBLLESRC)
    ```
