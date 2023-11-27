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
@Table(name = Constants.tableStore, schema = Constants.schemaManagement)
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
        private Long clientId;

        @Column(name = "id_tienda_tipo", nullable = false)
        private Long storeTypeId;

        @OneToOne
        @JoinColumn(name = "id_cliente", columnDefinition = "clientId", insertable = false)
        private Client client;

        @OneToMany
        @JoinColumn(name = "id_tienda_tipo", columnDefinition = "storeTypeId", insertable = false)
        private StoreType storeType;
}
