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
@Table(name = Constants.tableCancelledOrder,schema = Constants.schemaOrder)
public class CancelledOrder {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_orden_cancelada")
    private Long id;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "fecha_modificacion")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "id_orden")
    private Long orderingId;

    @Column(name = "id_razon_cancelacion")
    private Long cancellationReasonId;

    @Column(name = "id_cliente")
    private Long clientId;

    @Column(name = "usuario_token")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "id_orden",columnDefinition = "orderId", insertable = false,updatable = false)
    private Ordering ordering;

    @ManyToOne
    @JoinColumn(name = "id_razon_cancelacion", columnDefinition = "cancellationReasonId", insertable = false,updatable = false)
    private CancellationReason cancellationReason;

    @ManyToOne
    @JoinColumn(name = "id_cliente",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;
}
