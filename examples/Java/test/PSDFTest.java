import com.github.jmakino.Particle;

import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.TypeDescription;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;
import org.yaml.snakeyaml.nodes.Tag;
import org.yaml.snakeyaml.representer.Representer;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;

public class PSDFTest
{
    public static void main(String[] args) throws IOException {
        Constructor constructor = new Constructor();
        constructor.addTypeDescription(new TypeDescription(Particle.class, "!Particle"));

        DumperOptions options = new DumperOptions();
        options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);

        Representer representer = new SkipNullRepresenter();
        representer.addClassTag(Particle.class, new Tag("!Particle"));

        Yaml yaml = new Yaml(constructor, representer, options);

        InputStream input = new FileInputStream(new File("test.psdf"));
        for (Object data : yaml.loadAll(input)) {
            Particle p = (Particle)data;
            System.out.format("Read body with id = %s, t = %g, m = %g, r = {%g, %g, %g}, v = {%g, %g, %g}.\n",
                              p.id, p.t, p.m, p.r[0], p.r[1], p.r[2], p.v[0], p.v[1], p.v[2]);

            System.out.print("--- ");
            System.out.print(yaml.dump(p));
        }
    }
}
