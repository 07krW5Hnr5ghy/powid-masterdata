package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableOrderStock, schema = Constants.schemaStock)
public class OrderStock {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "order_stock_id")
    private Long id;

    @Column(name = "order_id")
    private Long orderId;

    @Column(name = "warehouse_id")
    private Long warehouseId;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "registration_date")
    private Date registrationDate;

    @Column(name = "update_date")
    private Date updateDate;

    @ManyToOne
    @JoinColumn(name = "order_id", columnDefinition = "orderId", insertable = false, updatable = false)
    private Ordering ordering;

    @ManyToOne
    @JoinColumn(name = "warehouse_id",columnDefinition = "warehouseId",insertable = false,updatable = false)
    private Warehouse warehouse;

}
