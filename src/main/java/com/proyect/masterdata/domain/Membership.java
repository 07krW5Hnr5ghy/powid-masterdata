package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableMembership, schema = Constants.schemaMaster)
public class Membership {
    @Id
    @GeneratedValue(generator = "sequence-membership")
    @GenericGenerator(
            name = "sequence-membership",
            strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
            parameters = {
                    @Parameter(name = "sequence_name", value = "menbresia_sequence"),
                    @Parameter(name = "initial_value", value = "1"),
                    @Parameter(name = "increment_size", value = "1")
            }
    )
    @Column(name = "id_menbresia", unique = true)
    private Long id;

    @Column(name = "id_modulo", unique = true)
    private Long idModule;

    @ManyToOne
    @JoinColumn(name = "id_modulo", columnDefinition = "idModule",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_modulo"))
    private Module module;
}
