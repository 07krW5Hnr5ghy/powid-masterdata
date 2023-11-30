package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

import org.hibernate.annotations.CreationTimestamp;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableMembership, schema = Constants.schemaPayment)
public class Membership {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "id_membresia")
        private Long id;

        @Column(name = "fecha_registro", nullable = false)
        private Date dateRegistration;

        @Column(name = "fecha_modificacion")
        @CreationTimestamp
        private Date dateUpdate;

        @Column(name = "fecha_vencimiento")
        @CreationTimestamp
        private Date expirationDate;

        @Column(name = "estado", nullable = false)
        private Boolean status;

        @Column(name = "demo", nullable = false)
        private Boolean demo;

        @Column(name = "id_cliente", nullable = false)
        private Long idClient;

        @Column(name = "id_subscribcion", nullable = false)
        private Long idSubscription;

        @ManyToOne
        @JoinColumn(name = "id_cliente", columnDefinition = "idClient", insertable = false, updatable = false)
        private Client client;

        @ManyToOne
        @JoinColumn(name = "id_subscribcion", columnDefinition = "idSubscription", insertable = false, updatable = false)
        private Subscription subscription;

}
