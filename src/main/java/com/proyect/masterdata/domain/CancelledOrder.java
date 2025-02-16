package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableCancelledOrder,schema = Constants.schemaOrder)
public class CancelledOrder {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "cancelled_order_id")
    private UUID id;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "order_id")
    private UUID orderingId;

    @Column(name = "cancellation_reason_id")
    private UUID cancellationReasonId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "user_id")
    private UUID userId;

    @ManyToOne
    @JoinColumn(name = "order_id",columnDefinition = "orderId", insertable = false,updatable = false)
    private Ordering ordering;

    @ManyToOne
    @JoinColumn(name = "cancellation_reason_id", columnDefinition = "cancellationReasonId", insertable = false,updatable = false)
    private CancellationReason cancellationReason;

    @ManyToOne
    @JoinColumn(name = "client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;
}
