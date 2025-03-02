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
@Table(name = Constants.tableOrderLog, schema = Constants.schemaOrder)
public class OrderLog {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "order_log_id")
    private UUID id;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "detail",columnDefinition = "text")
    private String detail;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "order_state_id")
    private UUID orderStateId;

    @Column(name = "order_id")
    private UUID orderId;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

    @ManyToOne()
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false,updatable = false)
    private Client client;

    @ManyToOne()
    @JoinColumn(name = "order_state_id",columnDefinition = "orderStateId",insertable = false,updatable = false)
    private OrderState orderState;

    @ManyToOne
    @JoinColumn(name = "order_id", columnDefinition = "orderId", insertable = false, updatable = false)
    private Ordering ordering;
}
