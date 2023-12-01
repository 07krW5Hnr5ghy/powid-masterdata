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
@Table(name = Constants.tableModule, schema = Constants.schemaMaster)
public class Module {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "id_modulo")
        private Long id;

        @Column(name = "nombre", nullable = false)
        private String name;

        @Column(name = "precio_mensual", nullable = false)
        private Double monthlyPrice;

        @Column(name = "estado", nullable = false)
        private Boolean status;

        @Column(name = "fecha_registro", nullable = false)
        @CreationTimestamp
        private Date registrationDate;

        @Column(name = "fecha_modificacion", nullable = false)
        @CreationTimestamp
        private Date updateDate;

        @Column(name = "usuario_token", nullable = false)
        private String tokenUser;

}
