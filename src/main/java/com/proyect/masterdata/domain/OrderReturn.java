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
@Table(name = Constants.tableOrderReturn, schema = Constants.schemaOrder)
public class OrderReturn {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "order_return_id")
    private String id;

    @Column(name = "registration_date")
    @CreationTimestamp()
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp()
    private OffsetDateTime updateDate;

    @Column(name = "token_user")
    private String tokenUser;

    @Column(name = "order_id")
    private Long orderId;

    @Column(name = "order_stock_id")
    private Long orderStockId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "status")
    private Boolean status;

    @ManyToOne()
    @JoinColumn(name = "order_id",columnDefinition = "orderId",insertable = false,updatable = false)
    private Ordering order;

    @ManyToOne()
    @JoinColumn(name = "order_stock_id",columnDefinition = "orderStockId",insertable = false,updatable = false)
    private OrderStock orderStock;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;
}
