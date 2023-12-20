package com.proyect.masterdata.domain;

import java.util.Date;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableStockReplenishment, schema = Constants.schemaInventory)
public class StockReplenishment {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_reposicion_inventario")
    private Long id;

    @Column(name = "fecha_registro")
    private Date registrationDate;

    @Column(name = "fecha_modificacion")
    private Date updateDate;

    @Column(name = "estado")
    private Boolean status;

    @Column(name = "id_producto")
    private Long productId;

    @ManyToOne
    @JoinColumn(name = "id_producto", columnDefinition = "productId", insertable = false, updatable = false)
    private Product product;

}
