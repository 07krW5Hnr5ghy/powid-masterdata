package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import java.util.Date;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableDiscount, schema = Constants.schemaManagement)
public class Discount {
        @Id
        @GeneratedValue(generator = "sequence-discount")
        @GenericGenerator(name = "sequence-discount", strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator", parameters = {
                        @Parameter(name = "sequence_name", value = "descuento_sequence"),
                        @Parameter(name = "initial_value", value = "1"),
                        @Parameter(name = "increment_size", value = "1")
        })
        @Column(name = "id_descuento", unique = true)
        private Long id;

        @Column(name = "descuento")
        private double discount;

        @Column(name = "estado")
        private boolean status;

        @Column(name = "fecha_registro")
        @CreationTimestamp
        private Date dateRegistration;

        @Column(name = "mes")
        private int months;

        @Column(name = "id_canal", unique = true)
        private Long idChannel;

        @ManyToOne
        @JoinColumn(name = "id_canal", columnDefinition = "idChannel", insertable = false, updatable = false)
        private Channel channel;
}
