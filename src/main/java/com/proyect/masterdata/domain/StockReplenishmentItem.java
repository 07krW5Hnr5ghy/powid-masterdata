package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableStockReplenishmentItem,schema = Constants.schemaStock)
public class StockReplenishmentItem {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "stock_replenishment_item_id")
    private Long id;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "order_id")
    private Long orderId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "product_id")
    private Long productId;

    @Column(name = "stock_replenishment_id")
    private Long stockReplenishmentId;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "order_id", columnDefinition = "orderId", insertable = false,updatable = false)
    private Ordering ordering;

    @ManyToOne
    @JoinColumn(name = "product_id", columnDefinition = "productId", insertable = false,updatable = false)
    private Product product;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "stock_replenishment_id",columnDefinition = "stockReplenishmentId",insertable = false,updatable = false)
    private StockReplenishment stockReplenishment;

}
