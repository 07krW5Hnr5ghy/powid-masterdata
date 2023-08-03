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
@Table(name = Constants.tableModule, schema = Constants.schemaMaster)
public class Module {
    @Id
    @GeneratedValue(generator = "sequence-generator")
    @GenericGenerator(
            name = "sequence-generator",
            strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
            parameters = {
                    @Parameter(name = "sequence_name", value = "tipo_usuario_modulo_sequence"),
                    @Parameter(name = "initial_value", value = "1"),
                    @Parameter(name = "increment_size", value = "1")
            }
    )
    @Column(name = "id_moduloa", unique = true)
    private Long id;

    @Column(name = "id_tipo_usuario_modulo", unique = true)
    private Long idUserTypeModule;

    @ManyToOne
    @JoinColumn(name = "id_tipo_usuario_modulo", columnDefinition = "idUserTypeModule",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_tipo_usuario_modulo"))
    private UserTypeModule userTypeModule;
}
