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
@NoArgsConstructor
@AllArgsConstructor
@Data
@Table(name = Constants.tableStockReturn, schema = Constants.schemaStock)
public class StockReturn {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "stock_return_id")
    private Long id;

    @Column(name = "serial")
    private String serial;

    @Column(name = "registration_date")
    private Date registrationDate;

    @Column(name = "update_date")
    private Date updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "purchase_id")
    private Long purchaseId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "purchase_id", columnDefinition = "purchaseId", insertable = false, updatable = false)
    private Purchase purchase;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

}
