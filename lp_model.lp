/* Objective function */
min: +0.375 C1 +0.2475 C2 +0.1275 C3 +0.12 C4 +0.105 C5 +0.09 C6 +0.0825 C7 +0.0525 C8 +0.0525 C9 +0.015 C10
 +0.89 C11 +0.57 C12 +0.41 C13 +0.31 C14 +0.23 C15 +0.17 C16 +0.17 C17 +0.4575 C18 +0.285 C19 +0.2025 C20
 +0.15 C21 +0.105 C22 +0.075 C23 +0.045 C24 -0.645 C25 -0.33 C26 -0.225 C27 -0.1575 C28 -0.15 C29 -0.1125 C30
 -0.0675 C31 -0.0375 C32 -0.015 C33 -0.89 C34 -0.57 C35 -0.41 C36 -0.31 C37 -0.23 C38 -0.17 C39 -0.17 C40
 -0.4575 C41 -0.285 C42 -0.2025 C43 -0.15 C44 -0.105 C45 -0.075 C46 -0.045 C47;

/* Constraints */
R1: +C25 = 1;
R2: +C1 = 1;
+C1 +C2 +C3 +C4 +C5 +C6 +C7 +C8 +C9 +C10 +C11 +C12 +C13 +C14 +C15 +C16 +C17 +C18 +C19 +C20 +C21 +C22
 +C23 +C24 >= 1;
+C1 +C2 +C3 +C4 +C5 +C6 +C7 +C8 +C9 +C10 +C11 +C12 +C13 +C14 +C15 +C16 +C17 +C18 +C19 +C20 +C21 +C22
 +C23 +C24 <= 4;
+C25 +C26 +C27 +C28 +C29 +C30 +C31 +C32 +C33 +C34 +C35 +C36 +C37 +C38 +C39 +C40 +C41 +C42 +C43 +C44 +C45
 +C46 +C47 >= 1;
+C25 +C26 +C27 +C28 +C29 +C30 +C31 +C32 +C33 +C34 +C35 +C36 +C37 +C38 +C39 +C40 +C41 +C42 +C43 +C44 +C45
 +C46 +C47 <= 4;
+C1 +C2 +C3 +C4 +C5 +C6 +C7 +C8 +C9 +C10 +C11 +C12 +C13 +C14 +C15 +C16 +C17 +C18 +C19 +C20 +C21 +C22
 +C23 +C24 -C25 -C26 -C27 -C28 -C29 -C30 -C31 -C32 -C33 -C34 -C35 -C36 -C37 -C38 -C39 -C40 -C41 -C42
 -C43 -C44 -C45 -C46 -C47 <= 3;
+C1 +C2 +C3 +C4 +C5 +C6 +C7 +C8 +C9 +C10 +C11 +C12 +C13 +C14 +C15 +C16 +C17 +C18 +C19 +C20 +C21 +C22
 +C23 +C24 -C25 -C26 -C27 -C28 -C29 -C30 -C31 -C32 -C33 -C34 -C35 -C36 -C37 -C38 -C39 -C40 -C41 -C42
 -C43 -C44 -C45 -C46 -C47 >= -3;
-510 C1 -232.5 C2 -33.75 C3 -30 C4 -26.25 C5 -19.95 C6 -19.05 C7 -7.65 C8 -6.45 C9 -0.75 C10 -1000 C11
 -430 C12 -195 C13 -72 C14 -35 C15 -19.8 C16 -19.8 C17 -750 C18 -322.5 C19 -146.25 C20 -54 C21 -26.25 C22
 -14.85 C23 -5.25 C24 +1275 C25 +405 C26 +187.5 C27 +64.5 C28 +57 C29 +27.75 C30 +13.35 C31 +0.75 C32
 +0.75 C33 +1000 C34 +430 C35 +195 C36 +72 C37 +35 C38 +19.8 C39 +19.8 C40 +750 C41 +322.5 C42 +146.25 C43
 +54 C44 +26.25 C45 +14.85 C46 +5.25 C47 <= -200;
-510 C1 -232.5 C2 -33.75 C3 -30 C4 -26.25 C5 -19.95 C6 -19.05 C7 -7.65 C8 -6.45 C9 -0.75 C10 -1000 C11
 -430 C12 -195 C13 -72 C14 -35 C15 -19.8 C16 -19.8 C17 -750 C18 -322.5 C19 -146.25 C20 -54 C21 -26.25 C22
 -14.85 C23 -5.25 C24 +1275 C25 +405 C26 +187.5 C27 +64.5 C28 +57 C29 +27.75 C30 +13.35 C31 +0.75 C32
 +0.75 C33 +1000 C34 +430 C35 +195 C36 +72 C37 +35 C38 +19.8 C39 +19.8 C40 +750 C41 +322.5 C42 +146.25 C43
 +54 C44 +26.25 C45 +14.85 C46 +5.25 C47 >= -1500;

/* Variable bounds */
C1 <= 1;
C2 <= 1;
C3 <= 1;
C4 <= 1;
C5 <= 1;
C6 <= 1;
C7 <= 1;
C8 <= 1;
C9 <= 1;
C10 <= 1;
C11 <= 1;
C12 <= 1;
C13 <= 1;
C14 <= 1;
C15 <= 1;
C16 <= 1;
C17 <= 1;
C18 <= 1;
C19 <= 1;
C20 <= 1;
C21 <= 1;
C22 <= 1;
C23 <= 1;
C24 <= 1;
C25 <= 1;
C26 <= 1;
C27 <= 1;
C28 <= 1;
C29 <= 1;
C30 <= 1;
C31 <= 1;
C32 <= 1;
C33 <= 1;
C34 <= 1;
C35 <= 1;
C36 <= 1;
C37 <= 1;
C38 <= 1;
C39 <= 1;
C40 <= 1;
C41 <= 1;
C42 <= 1;
C43 <= 1;
C44 <= 1;
C45 <= 1;
C46 <= 1;
C47 <= 1;

/* Integer definitions */
int C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17,C18,C19,C20,C21,C22,C23,C24,C25,C26,C27,C28,C29,C30,C31,C32,C33,C34,C35,C36,C37,C38,C39,C40,C41,C42,C43,C44,C45,C46,C47;
