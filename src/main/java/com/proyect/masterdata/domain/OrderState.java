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
@Table(name = Constants.tableOrderState, schema = Constants.schemaMaster)
public class OrderState {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "id_estado")
        private Long id;

        @Column(name = "nombre", length = 50, nullable = false)
        private String name;

        @Column(name = "estado", nullable = false)
        private Boolean status;

        @Column(name = "fecha_registro")
        @CreationTimestamp
        private Date dateRegistration;

        @Column(name = "fecha_modificacion")
        @CreationTimestamp
        private Date dateUpdate;

        @Column(name = "usuario_token", nullable = false)
        private String tokenUser;
}
