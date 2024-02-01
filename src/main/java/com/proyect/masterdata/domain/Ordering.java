package com.proyect.masterdata.domain;

import java.util.Date;

import org.hibernate.annotations.CreationTimestamp;

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
@Table(name = Constants.tableOrder, schema = Constants.schemaOrder)
public class Ordering {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_pedido")
    private Long id;

    @Column(name = "cancelacion")
    private Boolean cancellation;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "fecha_modificacion")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "id_estado_pedido")
    private Long orderStateId;

    @Column(name = "id_cliente")
    private Long clientId;

    @Column(name = "id_courier")
    private Long courierId;

    @Column(name = "usuario_token")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "id_estado_pedido", columnDefinition = "orderStateId", insertable = false, updatable = false)
    private OrderState orderState;

    @ManyToOne
    @JoinColumn(name = "id_cliente", columnDefinition = "clientId", insertable = false,updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "id_courier",columnDefinition = "courierId",insertable = false,updatable = false)
    private Courier courier;

}
