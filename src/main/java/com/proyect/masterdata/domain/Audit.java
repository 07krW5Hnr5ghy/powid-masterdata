package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.OffsetDateTime;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableAudit, schema = Constants.schemaManagement)
public class Audit {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "audit_id")
    private UUID id;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "audit_event_id")
    private UUID auditEventId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "reference")
    private String reference;

    @Column(name = "detail",columnDefinition = "text")
    private String detail;

    @ManyToOne
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

    @ManyToOne
    @JoinColumn(name="client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name="audit_event_id",columnDefinition = "auditEventId",insertable = false,updatable = false)
    private AuditEvent auditEvent;

}
