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
@Table(name = Constants.tableSizeType, schema = Constants.schemaMaster)
public class SizeType {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "id_tipo_talla")
        private Long id;

        @Column(name = "nombre", length = 50, nullable = false)
        private String name;

        @Column(name = "estado", nullable = false)
        private Boolean status;

        @Column(name = "fecha_registro")
        @CreationTimestamp
        private Date registrationDate;

        @Column(name = "fecha_modificacion")
        @CreationTimestamp
        private Date updateDate;

        @Column(name = "usuario_token", nullable = false)
        private String tokenUser;
}
