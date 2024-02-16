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
@Table(name = Constants.tableStockTransferItem,schema = Constants.schemaStock)
public class StockTransferItem {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "stock_transfer_item_id")
    private Long id;

    @Column(name = "supplier_product_id")
    private Long supplierProductId;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "registration_date")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "stock_transfer_id")
    private Long stockTransferId;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne()
    @JoinColumn(name = "supplier_product_id",columnDefinition = "supplierProductId",insertable = false,updatable = false)
    private SupplierProduct supplierProduct;

    @ManyToOne()
    @JoinColumn(name = "client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;

    @ManyToOne()
    @JoinColumn(name = "stock_transfer_id",columnDefinition = "stockTransferId",insertable = false,updatable = false)
    private StockTransfer stockTransfer;
}
