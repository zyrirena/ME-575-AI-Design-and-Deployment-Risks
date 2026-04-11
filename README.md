# ME-575-AI-Design-and-Deployment-Risks
Group 4: Risk Ready Final Project Repository


**1. Project Proposal Title and Mode Selected:**
<br>
  a. Title: FairHire AI - automated applicant screening tool 
  b. Governance heavy project

**2. Problem Context: **
<br>Our AI agent solves multiple problems in a recruiting environment.<br>  
Operational: it helps the business screen through hundreds of resumes quickly to pull minimum qualified candidates to review, it reviews education, experience and minimum requirements of the position.<br> 
Compliance: The AI agent includes data testing to provide bias review and ensure compliance with NY 144 law and compliance with EU AI Act.

<br>


**3. Target entity and use case: **
<br>Any business or organization that has an in-house Recruitment Office and screens applicants or a Recruitment Agency.  

<br>Stakeholders: 
<br>- Applicants
<br>- Hiring managers
<br>- Human Resource Specialists

**4. System Scope: **
<br>System boundary
<br>Model: will be running on Google Gemini 2.0 Flash

<br>Data: the system will include user uploaded resumes in a .docx or .pdf formats;  
<br>job descriptions that will include each positions required skills, experience, education, and certifications which will be stored in jobs table
<br>Initial test data from Kaggle
<br>Test resumes system generated for bias testing 
<br>
<br>
Workflow: <br>
file upload with mime/size validation → text extraction via pdf-parse or mammoth → PII scrubbing across 10 regex pattern categories → scrub validation → database persistence. Screening is then triggered separately: the anonymized text and job criteria are sent to Gemini, the structured JSON response is validated for prohibited content, scores are computed across four weighted dimensions (skills 40 pts, experience 35 pts, education 15 pts, certifications 10 pts), and results are stored with the full prompt logged.
<br>
<br>
AI Agents: <br>
Bias detection agent - generates six synthetic resumes with identical qualifications but varying demographic name signals, submits all six to Gemini, computes the Disparate Impact Ratio using the EEOC four-fifths rule, and stores results with an alert flag if DIR falls below 0.80.<br>
Selection rate monitor - continuous screening  after each evaluation or anomaly <br>
Audit logging agent - to create audit_logs for compliance
<br>
<br>
Users: AI agent will have assigned roles for administrators for bias testing, compliance PDF export, audit log access, and dataset loading; HR Recruiters to upload job descriptions, create new jobs, screen resumes, correct ratings/override ratings;  applicants to upload resumes, delete their profile, and submit questions.
<br>
<br>
Human review points: <br>
consent gate applicant must click “I consent” before resume is accepted; 
the recruiter override- to meet the human in the loop requirement the AI agent will require the recruiter to decide whether to advance, hold, reject, or escalate to human review, <br>
Applicant appeal - allow users to appeal AI decision so that recruiter can review and respond to the appeal <br>
bias alert review - when AI detects thresholds are exceeded the admin must manually review job criteria before continued use 
<br><br>
Outputs: 
<br>The FairHire AI outputs will include: 
<br>Applicant list with scores 
<br>Bias audit report
<br>NYC 144 Law compliance PDFs and exports of artifacts in JSON format 
<br>
**5. Risk framing: **

**6. Data Plan: **
<br>Database Used: Resume Dataset from Kaggle https://www.kaggle.com/datasets/snehaanbhawal/resume-dataset

**7. Tool stack: **

**8. Implementation plan: **

**9. Validation plan: **

**10. Deliverable and milestones: **

**11. Role Allocation: **

**12. Risks and fallback plan: **