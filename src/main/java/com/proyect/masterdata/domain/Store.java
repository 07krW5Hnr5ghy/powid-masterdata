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
@Table(name = Constants.tableStore, schema = Constants.schemaMaster)
public class Store {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "id_tienda", unique = true)
        private Long id;

        @Column(name = "nombre", nullable = false, length = 50, unique = true)
        private String name;

        @Column(name = "url", nullable = false, unique = true)
        private String url;

        @Column(name = "estado", columnDefinition = "BOOLEAN DEFAULT TRUE", nullable = false)
        private Boolean status;

        @Column(name = "fecha_registro", nullable = false)
        @CreationTimestamp
        private Date dateRegistration;

        @Column(name = "fecha_modificacion", nullable = false)
        @CreationTimestamp
        private Date dateUpdate;

        @Column(name = "id_cliente", unique = true, nullable = false)
        private Long idClient;
}
