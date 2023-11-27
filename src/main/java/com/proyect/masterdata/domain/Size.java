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
@Table(name = Constants.tableSize, schema = Constants.schemaMaster)
public class Size {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "id_talla")
        private Long id;

        @Column(name = "nombre", length = 50, nullable = false)
        private String name;

        @Column(name = "estado", nullable = false)
        private Boolean status;

        @Column(name = "fecha_registro", nullable = false)
        @CreationTimestamp
        private Date dateRegistration;

        @Column(name = "id_tipo_talla", nullable = false)
        private Long idSizeType;

        @ManyToOne
        @JoinColumn(name = "id_tipo_talla", columnDefinition = "idSizeType", insertable = false, updatable = false)
        private SizeType sizeType;

        @Column(name = "usuario", nullable = false)
        private String user;
}
