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

        @Column(name = "nombre", nullable = false, length = 50)
        private String name;

        @Column(name = "url", nullable = false)
        private String url;

        @Column(name = "estado", nullable = false)
        private Boolean status;

        @Column(name = "fecha_registro")
        @CreationTimestamp
        private Date dateRegistration;

        @Column(name = "fecha_modificacion")
        @CreationTimestamp
        private Date dateUpdate;

        @Column(name = "id_cliente", nullable = false)
        private Long clientId;

        @Column(name = "usuario_token", nullable = false)
        private String tokenUser;

        @Column(name = "id_tienda_tipo", nullable = false)
        private Long storeTypeId;

        @OneToOne
        @JoinColumn(name = "id_cliente", columnDefinition = "clientId", insertable = false, updatable = false)
        private Client client;

        @ManyToOne
        @JoinColumn(name = "id_tienda_tipo", columnDefinition = "storeTypeId", insertable = false, updatable = false)
        private StoreType storeType;
}
