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
@Table(name = Constants.tableMercadoPagoPayment, schema = Constants.schemaPayment)
public class MercadoPagoPayment {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "mercado_pago_payment_id", nullable = false)
    private Long id;

    @Column(name = "net_amount", nullable = false)
    private Double netAmount;

    @Column(name = "gross_amount", nullable = false)
    private Double grossAmount;

    @Column(name = "fee", nullable = false)
    private Double fee;

    @Column(name = "igv", nullable = false)
    private Double igv;

    @Column(name = "registration_date", nullable = false)
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "status", nullable = false)
    private Boolean status;

    @Column(name = "membership_id", nullable = false)
    private Long membershipId;

    @Column(name = "client_id")
    private Long clientId;

    @ManyToOne()
    @JoinColumn(name = "membership_id", columnDefinition = "membershipId", insertable = false, updatable = false)
    private Membership membership;

    @ManyToOne()
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

}
