package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.util.Date;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableAudit, schema = Constants.schemaManagement)
public class Audit {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "audit_id")
    private Long id;

    @Column(name = "user_id")
    private Long userId;

    @Column(name = "audit_event_id")
    private Long auditEventId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "token_user")
    private String tokenUser;

    @Column(name = "registration_date")
    @CreationTimestamp
    private Date registrationDate;

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
