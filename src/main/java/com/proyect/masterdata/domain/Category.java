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
@Table(name = Constants.tableCategory, schema = Constants.schemaMaster)
public class Category {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "id_categoria", unique = true)
        private Long id;

        @Column(name = "nombre", length = 50)
        private String name;

        @Column(name = "descripcion", length = 50)
        private String description;

        @Column(name = "estado", columnDefinition = "BOOLEAN DEFAULT TRUE")
        private Boolean status;

        @Column(name = "fecha_registro")
        @CreationTimestamp
        private Date registrationDate;

        @Column(name = "fecha_modificacion")
        @CreationTimestamp
        private Date updateDate;

        @Column(name = "usuario_token")
        private String tokenUser;
}
